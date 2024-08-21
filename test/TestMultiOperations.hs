module TestMultiOperations where

import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.MultiOperations as MultiOperations

allTests :: Test
allTests =
  TestList
    [test_iterateSubdirs]

test_iterateSubdirs :: Test
test_iterateSubdirs =
  TestLabel "iterate into subdirectories and run operation" $
    TestCase $ do
      let operation = MultiOperations.iterateVaultSubdirs D.dummyOperation
      let mock = mockWithVaultDir
      let result = runState (runExceptT $ operation) mock
      let mockAfterExec = snd result

      assertEqual
        "not really implemented"
        (Right ())
        (fst result)

      assertEqual
        "commands"
        []
        (execRecorded mockAfterExec)
