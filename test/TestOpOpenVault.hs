module TestOpOpenVault where

import Test.HUnit
import Control.Monad.State
import Control.Monad.Except
import System.Exit

import Data.Maybe

import MockSubstrate

import qualified Vaults.Base as V
import qualified Vaults.Substrate as Sub
import Vaults.OpOpenVault

-- TODO test scenarios:
-- succeed opening with partition name only
-- -- i.e. extension .vault added correctly
--
-- succeed forced opening in non-vault folder
-- fail forced opening when any vault already open

allTests :: Test
allTests = TestList [
      test_prerequisites
    , test_createLoopDevice
    , test_deleteLoopDevice
    , test_unlockDevice
    , test_mountDevice
    , test_parsingUdisksctlOutput
    , test_openVault
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "open in non-vault folder fails" $
    TestCase $ do
        let mock = emptyMock
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "non-vault folder" result
        assertNoExecCalls result,

    TestLabel "open when vault already open fails" $
    TestCase $ do
        let mock = mockWithActiveVault
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "vault already open" result
        assertNoExecCalls result,

    TestLabel "open without a partition filename fails" $
    TestCase $ do
        let mock = mockWithVault
        let params = ParamsOpenVault {
                partitionFilename = "",
                isForcedOpening = False
            }
        let result = runState (openVault params) mock
        assertOpError "partition filename is required" result
        assertNoExecCalls result
    ]

test_createLoopDevice :: Test
test_createLoopDevice = TestList [
    TestLabel "udisksctl loop-setup error prevents creating loop dev" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVault
        let result = runState (runExceptT $ createLoopDevice "/what") mock
        assertOpError "loop-setup failed" result,

    TestLabel "udisksctl loop-setup succeeds" $
    TestCase $ do
        let mock = addMockExecResult loopSetupOk mockWithVault
        let result = runState (runExceptT $ createLoopDevice "dummy.vault") mock
        assertEqual "loop-setup success" (Right "/dev/loop42") (fst result)
    ]

test_unlockDevice :: Test
test_unlockDevice = TestList [
    TestLabel "udisksctl unlock error prevents unlocking" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVault
        let result = runState (runExceptT $ unlockDevice "what") mock
        assertOpError "unlock failed" result,

    TestLabel "udisksctl unlock succeeds" $
    TestCase $ do
        let mock = addMockExecResult unlockOk mockWithVault
        let result = runState (runExceptT $ unlockDevice "what") mock
        assertEqual "unlock succeeds" (Right "/dev/dm-4") (fst result)
    ]

test_deleteLoopDevice :: Test
test_deleteLoopDevice = TestList [
    TestLabel "udisksctl loop-delete succeeds" $
    TestCase $ do
        let mock = addMockExecResult loopDeleteOk mockWithVault
        let result = runState (runExceptT $ deleteLoopDevice "/dev/loop42") mock
        assertEqual "loop-delete succeeds" (Right ()) (fst result)
    ]

test_mountDevice :: Test
test_mountDevice = TestList [
    TestLabel "udisksctl mount succeeds" $
    TestCase $ do
        let mock = addMockExecResult mountOk mockWithVault
        let result = runState (runExceptT $ mountDevice "/dev/dm-4") mock
        assertEqual "mount succeeds" (Right "/mnt/point") (fst result)
    ]

test_openVault :: Test
test_openVault = TestList [
    TestLabel "loop-setup error prevents opening" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVault
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "loop-setup failed" result
        let mockAfterExec = snd result
        assertEqual "only loop-setup was called"
            [("udisksctl", ["loop-setup", "-f", "local.vault"])]
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec)
        assertNoVaultEnvVar mockAfterExec,

    TestLabel "unlock error prevents opening and deletes loop device" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVault
                   where results = [loopSetupOk, unlockFail, loopDeleteOk]
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        let mockAfterExec = snd result
        assertOpError "unlock failed" result
        assertEqual "loop-setup, unlock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop42"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec)
        assertNoVaultEnvVar mockAfterExec,

    TestLabel "mount error prevents opening and undoes unlock and loop-setup" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVault
                   where results = [loopSetupOk, unlockOk, mountFail, lockOk, loopDeleteOk]
                         loopSetupOk  = Sub.ExecResult ExitSuccess loopSetupOut ""
                         unlockOk     = Sub.ExecResult ExitSuccess unlockOut ""
                         mountFail    = Sub.ExecResult (ExitFailure 16) "" "didnt work"
                         lockOk       = Sub.ExecResult ExitSuccess "Locked /dev/dm-4." ""
                         loopDeleteOk = Sub.ExecResult ExitSuccess "" ""
                         loopSetupOut = "Mapped file local.vault as /dev/loop42."
                         unlockOut    = "Unlocked /dev/loop42 as /dev/dm-4."
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        let mockAfterExec = snd result
        assertOpError "mount failed" result
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["mount", "-b", "/dev/dm-4"])
            , ("udisksctl", ["lock", "-b", "/dev/dm-4"])
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop42"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec)
        assertNoVaultEnvVar mockAfterExec,

    TestLabel "mount succeeds, no inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVault
                   where results = [loopSetupOk, unlockOk, mountOk]
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        let mockAfterExec = snd result
        assertEqual "mount succeeds" (Right ()) (fst result)
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["mount", "-b", "/dev/dm-4"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "current directory changed to mountpoint"
            "/mnt/point"
            (currentDir mockAfterExec)

        let vri = V.VaultRuntimeInfo {
                  V.srcDir = "/home/user",
                  V.loopDev = "/dev/loop42",
                  V.mapperDev = "/dev/dm-4",
                  V.mountedRepo = "/mnt/point",
                  V.partition = "local.vault",
                  V.partitionLocation = V.LocalPartition
              }
        assertActiveVaultEnvVarSet vri mockAfterExec,

    TestLabel "mount succeeds, vault has inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [loopSetupOk, unlockOk, mountOk]
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        let mockAfterExec = snd result
        assertEqual "mount succeeds" (Right ()) (fst result)
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["mount", "-b", "/dev/dm-4"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "prev directory is mountpoint"
            "/mnt/point"
            (prevDir mockAfterExec)
        assertEqual "current directory changed to repo within mountpoint"
            "repo"
            (currentDir mockAfterExec)

        let vri = V.VaultRuntimeInfo {
                  V.srcDir = "/home/user",
                  V.loopDev = "/dev/loop42",
                  V.mapperDev = "/dev/dm-4",
                  V.mountedRepo = "/mnt/point/repo",
                  V.partition = "local.vault",
                  V.partitionLocation = V.LocalPartition
              }
        assertActiveVaultEnvVarSet vri mockAfterExec

    ]

test_parsingUdisksctlOutput :: Test
test_parsingUdisksctlOutput = TestList [
    TestLabel "parsing output of loop-setup" $
    TestCase $ do
        let output = ""
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped dummy.vault as /dev/loop42."
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped file dummy.vault as ."
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped file dummy.vault as /dev/lo.op42."
        parseOutputLoopSetup output @?= invalidOutput
        let output = "Mapped file dummy.vault as /dev/loop42"
        parseOutputLoopSetup output @=? invalidOutput
        let output = "Mapped file dummy.vault as /dev/loop42."
        parseOutputLoopSetup output @=? (Right "/dev/loop42"),

    TestLabel "parsing output of unlock" $
    TestCase $ do
        let output = ""
        parseOutputUnlock output @?= invalidOutput
        let output = "Unlocked /dev/loop42 as /dev/dm-4"
        parseOutputUnlock output @?= invalidOutput
        let output = "Unlocked /dev/loop42 as /dev/dm-4."
        parseOutputUnlock output @?= (Right "/dev/dm-4"),

    TestLabel "parsing output of mount" $
    TestCase $ do
        let output = ""
        parseOutputMount output @?= invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/point."
        parseOutputMount output @?= invalidOutput
        let output = "Mounted /dev/dm-4 as /mnt/point"
        parseOutputMount output @?= (Right "/mnt/point")

    ]

loopSetupOk  = Sub.ExecResult ExitSuccess loopSetupOut ""
loopSetupFail = Sub.ExecResult (ExitFailure 16) "" "didnt work"
loopDeleteOk = Sub.ExecResult ExitSuccess "" ""
unlockOk     = Sub.ExecResult ExitSuccess unlockOut ""
unlockFail   = Sub.ExecResult (ExitFailure 16) "" "didnt work"
mountOk      = Sub.ExecResult ExitSuccess mountOut ""
loopSetupOut = "Mapped file local.vault as /dev/loop42."
unlockOut    = "Unlocked /dev/loop42 as /dev/dm-4."
mountOut     = "Mounted /dev/dm-4 at /mnt/point"

assertOpError :: (Eq a, Show a) => String -> (Either String a, Mock) -> IO ()
assertOpError err (opResult, _) =
    assertEqual err (Left err) opResult

assertNoExecCalls :: (V.OpResult, Mock) -> IO ()
assertNoExecCalls (_, mock) =
    assertEqual "no exec calls" 0 (nExecs mock)

assertNoVaultEnvVar :: Mock -> IO ()
assertNoVaultEnvVar mock =
    assertEqual "no vault env var"
        Nothing
        (lookup key $ envVars mock)
    where key = V.activeVaultEnvName

assertVaultEnvVarSet :: Mock -> IO ()
assertVaultEnvVarSet mock =
    assertBool "vault env var set" (isJust mEnvVar)
    where mEnvVar = (lookup key $ envVars mock)
          key = V.activeVaultEnvName

assertActiveVaultEnvVarSet :: V.VaultRuntimeInfo -> Mock -> IO ()
assertActiveVaultEnvVarSet vri mock = do
    let key = V.activeVaultEnvName
    let mEnvVar = (lookup key $ envVars mock)
    case mEnvVar of
         Nothing -> assertFailure "no vault env var"
         Just envVar -> assertEqual "active vault as expected" vri (read envVar)

mkOpenVault :: String -> ParamsOpenVault
mkOpenVault fname = ParamsOpenVault {
    partitionFilename = fname,
    isForcedOpening = False
}

emptyMock :: Mock
emptyMock = Mock {
      currentDir = "/home/user"
    , prevDir = "/"
    , hasVaultDir = False
    , hasRepoDir = False
    , envVars = []
    , nExecs = 0
    , execRecorded = []
    , execResults = []
    }

mockWithVault :: Mock
mockWithVault = emptyMock {
    hasVaultDir = True
}

mockWithVaultAndRepoDir :: Mock
mockWithVaultAndRepoDir = emptyMock {
      hasVaultDir = True
    , hasRepoDir = True
}

mockWithActiveVault :: Mock
mockWithActiveVault = mockWithVault {
      hasVaultDir = True
    , envVars = [(V.activeVaultEnvName, "some_vault")]
}
