module TestOperations_Setup where

import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.Operations as Operations

allTests :: Test
allTests =
  TestList
    [ test_setup_verification
    ]

test_setup_verification :: Test
test_setup_verification =
  TestCase $ do
    let operation = Operations.doSetupVault mockVaultInfo
    let mock = addMockExecResults results mockWithVaultAndRepoDir
          where
            results =
              ( D.openPartitionExecOk
                  ++ [D.gitLogExec True]
                  ++ D.closePartitionExecOk
              )
                <*> (pure D.localOp)
    let result = runState (runExceptT $ operation) mock
    let mockAfterExec = snd result
    assertEqual "vault opened, set up, closed" (Right ()) (fst result)
