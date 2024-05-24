module TestOperations_ShellPartition where

import Control.Monad.State
import Control.Monad.Except

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

import qualified Vaults.Operations as Operations
import Vaults.Open

allTests :: Test
allTests = TestList [
      test_prerequisites
    , test_shellPartition
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "open without a partition filename fails" $
    TestCase $ do
        let mock = emptyMock
        let result = runState (runExceptT $ openPartition "") mock
        let mockAfterExec = snd result
        assertOpError "partition filename is required" result
        assertNoExecCalls mockAfterExec
    ]

test_shellPartition :: Test
test_shellPartition =
    TestLabel "open shell in mounted partition, then close shell" $
    TestCase $ do
        let operation = Operations.doShellPartition "local.vault"
        let mock = addMockExecResults results emptyMock
                   where results = (  D.openPartitionExecOk
                                   ++ D.closePartitionExecOk
                                   ) <*> (pure D.localOp)
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result
        assertEqual "vault opened, shell started and exited, closed" (Right()) (fst result)
