module TestOperations where

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Operations as Operations

import Test.HUnit
import Assertions
import MockSubstrate

allTests :: Test
allTests = TestList [
    test_editSuccessful
    ]

test_editSuccessful :: Test
test_editSuccessful =
    TestLabel "vault opened, edited, closed" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [loopSetupOk, unlockOk, mountOk,
                                    unmountOk, lockOk, loopDeleteOk]
        let result = runState (runExceptT $ Operations.doEditVault mockVaultInfo) mock
        assertEqual "edit successful" (Right()) (fst result)
        assertFailure "test fail"
