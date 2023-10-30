module TestOpOpenVault where

import Test.HUnit
import Control.Monad.State
import Control.Monad.Except
import System.Exit

import MockSubstrate
import Assertions

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
    , test_openVault
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "open in non-vault folder fails" $
    TestCase $ do
        let mock = emptyMock
        let params = mkParamsOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "non-vault folder" result
        assertNoExecCalls result,

    TestLabel "open when vault already open fails" $
    TestCase $ do
        let mock = mockWithActiveVault
        let params = mkParamsOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "vault already open" result
        assertNoExecCalls result,

    TestLabel "open without a partition filename fails" $
    TestCase $ do
        let mock = mockWithVaultDir
        let params = ParamsOpenVault {
                partitionFilename = "",
                isForcedOpening = False
            }
        let result = runState (openVault params) mock
        assertOpError "partition filename is required" result
        assertNoExecCalls result
    ]

test_openVault :: Test
test_openVault = TestList [
    TestLabel "loop-setup error prevents opening" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVaultDir
        let params = mkParamsOpenVault "local.vault"
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
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockFail, loopDeleteOk]
        let params = mkParamsOpenVault "local.vault"
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
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockOk, mountFail, lockOk, loopDeleteOk]
        let params = mkParamsOpenVault "local.vault"
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
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockOk, mountOk]
        let params = mkParamsOpenVault "local.vault"
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
        let params = mkParamsOpenVault "local.vault"
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

mkParamsOpenVault :: String -> ParamsOpenVault
mkParamsOpenVault fname = ParamsOpenVault {
    partitionFilename = fname,
    isForcedOpening = False
}
