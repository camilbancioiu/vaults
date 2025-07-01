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
    let mock = emptyMock
    let result = runState (runExceptT $ openPartition "") mock
    let mockAfterExec = snd result
    assertOpError "partition filename is required" result
    assertNoExecCalls mockAfterExec

test_shellPartition_ok :: Test
test_shellPartition_ok =
  TestLabel "open shell in mounted partition, then close shell" $
    TestCase $ do
      let operation = Operations.doShellPartition "local.vault"
      let mock = addMockExecResults results emptyMock
            where
              results =
                ( D.openPartitionExecOk
                    ++ D.closePartitionExecOk
                )
                  <*> (pure D.localOp)
      let result = runState (runExceptT $ operation) mock
      let mockAfterExec = snd result
      assertEqual "vault opened, shell started and exited, closed" (Right ()) (fst result)
