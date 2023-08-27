module TestVaults where

import Test.HUnit

import qualified Vaults
import Substrate
import MockSubstrate

import Control.Monad.State

allTests :: Test
allTests = TestList [ test_isVaultDir ]

test_isVaultDir :: Test
test_isVaultDir = TestCase $ do
    let mock = mockWithVaultDir False
    let isV = evalState Vaults.isVaultDir mock
    assertEqual "isVaultDir" False isV

    let mock = mockWithVaultDir True
    let isV = evalState Vaults.isVaultDir mock
    assertEqual "isVaultDir" True isV

test_loadVault :: Test
test_loadVault = TestCase $ do
    let mock = Mock { hasVaultDir = True }
    return ()

mockWithVaultDir :: Bool -> Mock
mockWithVaultDir d = Mock { hasVaultDir = d }

