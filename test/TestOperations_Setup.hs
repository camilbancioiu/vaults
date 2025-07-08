module TestOperations_Setup where

import Test.HUnit

allTests :: Test
allTests =
  TestList
    [ test_setup_verification
    ]

test_setup_verification :: Test
test_setup_verification =
  TestCase $ do
    assertFailure "unimplemented"
