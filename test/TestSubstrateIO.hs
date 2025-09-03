module TestSubstrateIO where

import Control.Exception
import Control.Monad.Except
import SubstrateIO
import Test.HUnit

allTests :: Test
allTests = TestList [test_exceptionAdapter]

data DummyException = DummyException deriving (Show, Eq)

instance Exception DummyException

test_exceptionAdapter :: Test
test_exceptionAdapter =
  TestLabel "test_exceptionAdapter" $ TestCase $ do
    adapted <- runExceptT $ adaptException dummyIOAction
    let expected = Left "DummyException"
    assertEqual
      "adapted exception"
      expected
      adapted

dummyIOAction :: IO ()
dummyIOAction = throw DummyException
