module TestRepo where

import Test.HUnit

allTests :: Test
allTests = TestList [test_MissingRepoDir]

test_MissingRepoDir :: Test
test_MissingRepoDir = TestCase $ do
  assertFailure "not implemented"
