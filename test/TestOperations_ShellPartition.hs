module TestOperations_ShellPartition where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import Vaults.Open
import qualified Vaults.Operations as Operations

-- TODO add test where the shell crashes

allTests :: Test
allTests =
  TestList
    [ test_shellPartition_without_partition_file,
      test_shellPartition_ok
    ]

test_shellPartition_without_partition_file :: Test
test_shellPartition_without_partition_file =
  TestCase $ do
    let operation = openPartition ""
    let mock = emptyMock
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    assertOpError
      "partition filename is required"
      operationResult

    assertNoExecCalls mockAfterExec

test_shellPartition_ok :: Test
test_shellPartition_ok =
  TestLabel "open shell in mounted partition, then close shell" $
    TestCase $ do
      let operation = Operations.doShellPartition "local.vault"

      let mockExecResults =
            ( D.openPartitionExecOk
                ++ D.closePartitionExecOk
            )
              <*> (pure D.localOp)

      let mock = addMockExecResults mockExecResults emptyMock
      let operationResult = runState (runExceptT $ operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "vault opened, shell started and exited, closed"
        (Right ())
        (fst operationResult)
