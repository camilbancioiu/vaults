module TestVaults where

import Test.HUnit

import qualified Vaults
import Substrate
import MockSubstrate

import Control.Monad.Identity

allTests :: Test
allTests = TestList [ test_isVaultDir ]

test_isVaultDir :: Test
test_isVaultDir = TestCase $ do
    let mock = return mockedVault :: Identity MockSubstrate

    let v = runIdentity (mock >> Vaults.isVaultDir)
    v @?= True
