module TestMultiOperations where

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
      assertFailure "not implemented"
