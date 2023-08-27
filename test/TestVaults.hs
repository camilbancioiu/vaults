module TestVaults where

import Test.HUnit

import Data.Maybe
import Control.Monad.State

import qualified Vaults as V
import Substrate
import MockSubstrate

allTests :: Test
allTests = TestList [
    test_isVaultDir,
    test_loadVault,
    test_isAnyVaultActive,
    test_getActiveVault
    ]

test_isVaultDir :: Test
test_isVaultDir = TestCase $ do
    let mock = emptyMock
    let isV = evalState V.isVaultDir mock
    assertEqual "isVaultDir" False isV

    let mock = mockWithVaultDir
    let isV = evalState V.isVaultDir mock
    assertEqual "isVaultDir" True isV

test_loadVault :: Test
test_loadVault = TestCase $ do
    let mock = mockWithVaultDir
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
    let mock = emptyMock
    let active = evalState V.isAnyVaultActive mock
    assertBool "no vault active" (not active)

    let var = (V.activeVaultEnvName, "something")
    let mock = mockWithEnvVar var
    let active = evalState V.isAnyVaultActive mock
    assertBool "vault is active" active

test_getActiveVault :: Test
test_getActiveVault = TestCase $ do
    let mock = emptyMock
    let vri = evalState V.getActiveVault mock
    assertEqual "no vault active" Nothing vri

    let mockVRI = mockVaultRuntimeInfo
    let var = (V.activeVaultEnvName, show mockVRI)
    let mock = mockWithEnvVar var
    let vri = evalState V.getActiveVault mock
    assertEqual "loaded vault" (Just mockVRI) vri

emptyMock :: Mock
emptyMock = Mock {
    hasVaultDir = False,
    envVars = []
    }

mockWithVaultDir :: Mock
mockWithVaultDir = emptyMock {
    hasVaultDir = True
    }

mockWithEnvVar :: (String, String) -> Mock
mockWithEnvVar var = Mock {
    hasVaultDir = False,
    envVars = [var]
}
