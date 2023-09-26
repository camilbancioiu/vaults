module TestOpOpenVault where

import Test.HUnit
import Control.Monad.State

import MockSubstrate

import qualified Vaults.Base as V
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
                partitionFilename = Nothing,
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
        assertFailure "test not implemented"
    ]

assertOpError :: String -> (V.OpResult, Mock) -> IO ()
assertOpError err (opResult, _) =
    assertEqual err (Left err) opResult

assertNoExecCalls :: (V.OpResult, Mock) -> IO ()
assertNoExecCalls (_, mock) =
    assertEqual "no exec calls" 0 (nExecs mock)

mkOpenVault :: String -> ParamsOpenVault
mkOpenVault fname = ParamsOpenVault {
    partitionFilename = Just fname,
    isForcedOpening = False
}

emptyMock :: Mock
emptyMock = Mock {
    hasVaultDir = False,
    envVars = [],
    nExecs = 0
    }

mockWithVault :: Mock
mockWithVault = Mock {
    hasVaultDir = True,
    envVars = [],
    nExecs = 0
}

mockWithActiveVault :: Mock
mockWithActiveVault = Mock {
    hasVaultDir = True,
    envVars = [(V.activeVaultEnvName, "some_vault")],
    nExecs = 0
}
