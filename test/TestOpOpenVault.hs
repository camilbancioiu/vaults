module TestOpOpenVault where

import Test.HUnit
import Control.Monad.State
import Control.Monad.Except
import System.Exit

import MockSubstrate

import qualified Vaults.Base as V
import qualified Vaults.Substrate as Sub
import Vaults.OpOpenVault

-- TODO test scenarios:
-- fail when loop-setup fails
-- -- assert returned error
-- -- assert no other calls to udisksctl
-- fail when unlock fails
-- -- e.g. wrong passphrase
-- -- assert loop-delete called
-- -- assert no other calls to udisksctl
-- fail when mounting fails fails
-- -- assert lock called
-- -- assert loop-delete called
-- -- assert no other calls to udisksctl
--
-- succeed opening vault in correct conditions
-- -- assert cwd
-- -- assert active vault runtime info in env
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
    TestLabel "udisksctl loop-setup error fails" $
    TestCase $ do
        let mock = addMockExecResult er mockWithVault
                   where er = Sub.ExecResult (ExitFailure 16) "" "didnt work"
        let result = runState (runExceptT $ createLoopDevice "/what") mock
        assertOpError "loop-setup failed" result,

    TestLabel "udisksctl loop-setup succeeds and returns device path" $
    TestCase $ do
        let mock = addMockExecResult loopSetupOk mockWithVault
                   where loopSetupOk = Sub.ExecResult ExitSuccess outStr ""
                         outStr = "Mapped file dummy.vault as /dev/loop42."
        let result = runState (runExceptT $ createLoopDevice "dummy.vault") mock
        assertEqual "loop-setup success" (Right "/dev/loop42") (fst result)
    ]


test_unlockDevice :: Test
test_unlockDevice = TestList [
    TestLabel "udisksctl unlock error fails" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVault
                   where loopSetupFail = Sub.ExecResult (ExitFailure 16) "" "didnt work"
        let result = runState (runExceptT $ unlockDevice "what") mock
        assertOpError "unlock failed" result,

    TestLabel "udisksctl unlock succeeds" $
    TestCase $ do
        let mock = addMockExecResult unlockOk mockWithVault
                   where unlockOk = Sub.ExecResult ExitSuccess outStr ""
                         outStr = "Unlocked /dev/loop24 as /dev/dm-8."
        let result = runState (runExceptT $ unlockDevice "what") mock
        assertEqual "unlock succeeds" (Right "/dev/dm-8") (fst result)
    ]

test_deleteLoopDevice :: Test
test_deleteLoopDevice = TestList [
    TestLabel "udisksctl loop-delete succeeds" $
    TestCase $ do
        let mock = addMockExecResult loopDeleteOk mockWithVault
                   where loopDeleteOk = Sub.ExecResult ExitSuccess "undefined" ""
        let result = runState (runExceptT $ deleteLoopDevice "/dev/loop42") mock
        assertEqual "loop-delete succeeds" (Right ()) (fst result)
 ]

test_openVault :: Test
test_openVault = TestList [
    TestLabel "loop-setup error fails opening" $
    TestCase $ do
        let mock = addMockExecResult er mockWithVault
                   where er = Sub.ExecResult (ExitFailure 16) "" "didnt work"
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "loop-setup failed" result
        let mockAfterExec = snd result
        assertEqual "only loop-setup was called"
            [("udisksctl", ["loop-setup", "-f", "local.vault"])]
            (execRecorded mockAfterExec),

    TestLabel "unlock error fails opening and deletes loop device" $
    TestCase $ do
        let mock = addMockExecResults ers mockWithVault
                   where ers = [loopSetupOk, unlockFail, loopDeleteOk]
                         loopSetupOk = Sub.ExecResult ExitSuccess loopSetupOut ""
                         loopSetupOut = "Mapped file local.vault as /dev/loop42."
                         unlockFail = Sub.ExecResult (ExitFailure 16) "" "didnt work"
                         loopDeleteOk = Sub.ExecResult ExitSuccess "" ""
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
        parseOutputUnlock output @?= (Right "/dev/dm-4")

    ]

assertOpError :: (Eq a, Show a) => String -> (Either String a, Mock) -> IO ()
assertOpError err (opResult, _) =
    assertEqual err (Left err) opResult

assertNoExecCalls :: (V.OpResult, Mock) -> IO ()
assertNoExecCalls (_, mock) =
    assertEqual "no exec calls" 0 (nExecs mock)

mkOpenVault :: String -> ParamsOpenVault
mkOpenVault fname = ParamsOpenVault {
    partitionFilename = fname,
    isForcedOpening = False
}

emptyMock :: Mock
emptyMock = Mock {
      hasVaultDir = False
    , envVars = []
    , nExecs = 0
    , execRecorded = []
    , execResults = []
    }

mockWithVault :: Mock
mockWithVault = emptyMock {
      hasVaultDir = True
}

mockWithActiveVault :: Mock
mockWithActiveVault = mockWithVault {
      hasVaultDir = True
    , envVars = [(V.activeVaultEnvName, "some_vault")]
}
