module TestVaults where

import Test.HUnit

import qualified Vaults as V
import Substrate
import MockSubstrate

import Control.Monad.State

allTests :: Test
allTests = TestList [ test_isVaultDir, test_loadVault ]

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
        V.name = "dummy",
        V.localname = "local",
        V.remotes = ["remoteA", "remoteB"],
        V.remoteStore = "ssh://remoteStore"
        }
    let v = evalState V.loadVault mock
    assertEqual "loadVault" expected v

mockWithVaultDir :: Bool -> Mock
mockWithVaultDir d = Mock { hasVaultDir = d }
