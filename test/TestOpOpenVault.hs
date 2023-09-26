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
        assertNoExecCalls result,


test_createLoopDevice :: Test
test_createLoopDevice = TestList [
    TestLabel "udisksctl loop-setup error fails" $
    TestCase $ do
        let mock = addMockExecResult er mockWithVault
                   where er = Sub.ExecResult {
                         Sub.exitCode = ExitFailure 16
                       , Sub.output = ""
                       , Sub.errorOutput = "didnt work"
                   }

        let result = runState (runExceptT $ createLoopDevice "/what") mock
        assertOpError "loop-setup failed" result
    ]


test_openVault :: Test
test_openVault = TestList [
    TestLabel "loop-setup error fails opening" $
    TestCase $ do
        let mock = addMockExecResult er mockWithVault
                   where er = Sub.ExecResult {
                         Sub.exitCode = ExitFailure 16
                       , Sub.output = ""
                       , Sub.errorOutput = "didnt work"
                   }
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "loop-setup failed" result
        let mock = snd result
        assertEqual "only loop-setup was called"
            [("udisksctl", ["loop-setup", "-f", "local.vault"])]
            (execRecorded mock),


    TestLabel "unlock error fails opening and deletes loop device" $
    TestCase $ do
        let mock = addMockExecResult er mockWithVault
                   where er = Sub.ExecResult {
                         Sub.exitCode = ExitSuccess
                       , Sub.output = ""
                       , Sub.errorOutput = ""
                   }
        let mock = addMockExecResult er mockWithVault
                   where er = Sub.ExecResult {
                         Sub.exitCode = ExitFailure 16
                       , Sub.output = ""
                       , Sub.errorOutput = "didnt work"
                   }

        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "unlock failed" result
        let mock = snd result
        assertEqual "loop-setup, unlock, loop-delete were called"
            [("udisksctl", ["loop-setup", "-f", "local.vault"])]
            (execRecorded mock)
    ]


assertOpError :: String -> (V.OpResult, Mock) -> IO ()
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
