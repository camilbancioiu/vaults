module TestVaultOpen where

import Test.HUnit
import Control.Monad.State

import MockSubstrate

import qualified Vaults.Base as V
import Vaults.Operations (openVault, OpResult)
import qualified Vaults.OperationParams as P

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
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "open in non-vault folder fails" $
    TestCase $ do
        let mock = emptyMock
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "non-vault folder" result,

    TestLabel "open when vault already open fails" $
    TestCase $ do
        let mock = mockWithActiveVault
        let params = mkOpenVault "local.vault"
        let result = runState (openVault params) mock
        assertOpError "vault already open" result
    ]

-- test_loopSetupFails :: Test
-- test_loopSetupFails = TestCase $ do
--     assertFailure "test not implemented"

assertOpError :: String -> (OpResult, Mock) -> IO ()
assertOpError err (opResult, mock) = do
    assertEqual err (Left err) opResult
    assertEqual "no exec calls" 0 (nExecs mock)

mkOpenVault :: String -> P.OpenVault
mkOpenVault fname = P.OpenVault {
    P.partitionFilename = Just fname,
    P.isForcedOpening = False
}

emptyMock :: Mock
emptyMock = Mock {
    hasVaultDir = False,
    envVars = [],
    nExecs = 0
    }

mockWithActiveVault :: Mock
mockWithActiveVault = Mock {
    hasVaultDir = True,
    envVars = [(V.activeVaultEnvName, "some_vault")],
    nExecs = 0
}
