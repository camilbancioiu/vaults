module TestVaults where

import Test.HUnit

import qualified Vaults as V
import Substrate
import MockSubstrate

import Control.Monad.State

allTests :: Test
allTests = TestList [
    test_isVaultDir,
    test_loadVault,
    test_isAnyVaultActive
    ]

test_isVaultDir :: Test
test_isVaultDir = TestCase $ do
    let mock = mockWithVaultDir False
    let isV = evalState V.isVaultDir mock
    assertEqual "isVaultDir" False isV

    let mock = mockWithVaultDir True
    let isV = evalState V.isVaultDir mock
    assertEqual "isVaultDir" True isV

test_loadVault :: Test
test_loadVault = TestCase $ do
    let mock = mockWithVaultDir True
    let expected = V.Vault {
        V.name = "mockVault",
        V.localname = "local",
        V.remotes = ["remoteA", "remoteB"],
        V.remoteStore = "ssh://remoteStore"
        }
    let v = evalState V.loadVault mock
    assertEqual "loadVault" expected v

test_isAnyVaultActive :: Test
test_isAnyVaultActive = TestCase $ do
    let mock = mockWithVaultDir True
    let isActive = evalState V.isAnyVaultActive mock
    assertBool "no vault active" (not isActive)

    let var = (V.activeVaultEnvName, "something")
    let mock = mockWithEnvVar var
    let isActive = evalState V.isAnyVaultActive mock
    assertBool "vault is active" isActive

mockWithVaultDir :: Bool -> Mock
mockWithVaultDir d = Mock {
    hasVaultDir = d,
    envVars = []
    }

mockWithEnvVar :: (String, String) -> Mock
mockWithEnvVar var = Mock {
    hasVaultDir = True,
    envVars = [var]
}
