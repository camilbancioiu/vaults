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
    let mock = MockSubstrate { hasVaultDir = False }
    let isV = evalState Vaults.isVaultDir mock
    assertEqual "isVaultDir" False isV

    let mock = MockSubstrate { hasVaultDir = True }
    let isV = evalState Vaults.isVaultDir mock
    assertEqual "isVaultDir" True isV
