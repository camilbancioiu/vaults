module TestMkRepo where

import Test.HUnit

allTests :: Test
allTests = TestList [
    test_parseGitRemotes
    ]

test_parseGitRemotes :: Test
test_parseGitRemotes =
    TestLabel "parse output of `git remotes -v`" $
    TestCase $ do
        assertFailure "not implemented"
