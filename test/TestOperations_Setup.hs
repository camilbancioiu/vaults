module TestOperations_Setup where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.Operations as Operations

allTests :: Test
allTests =
  TestList
    [ test_setup_verification_ok
    ]

test_setup_verification_ok :: Test
test_setup_verification_ok =
  TestCase $ do
    let operation = Operations.doSetupVault mockVaultInfo

    let expectedCommands =
          []
            ++ D.preOpenPartitionCmds
            ++ D.openPartitionCmds D.localOp
            ++ D.postOpenPartitionCmds D.localOp
            ++ [D.changeToRepoDir D.localOp]
            ++ D.verifyRepoCmds
            ++ [D.gitLogCmd]
            ++ D.preClosePartitionCmds
            ++ D.closePartitionCmds D.localOp
            ++ [("writeFile", ["local.log"])]

    let mock = addMockExecResults results mockWithVaultAndRepoDir
          where
            results =
              []
                ++ (D.openPartitionExecOk <*> (pure D.localOp))
                ++ D.successfulRepoVerificationExecResults
                ++ [D.gitLogExec True D.localOp]
                ++ (D.closePartitionExecOk <*> (pure D.localOp))

    let result = runState (runExceptT $ operation) mock
    let mockAfterExec = snd result

    assertEqual "vault opened, set up, closed" (Right ()) (fst result)

    assertEqual
      "all commands executed"
      expectedCommands
      (execRecorded mockAfterExec)
    assertAllExecsConsumed mockAfterExec
