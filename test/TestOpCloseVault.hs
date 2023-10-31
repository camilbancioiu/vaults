module TestOpCloseVault where

import Test.HUnit

allTests :: Test
allTests = TestList [
      test_prerequisites
    , test_closeVault
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "closing fails when no active vault" $
    TestCase $ do
        assertFailure "not implemented"

    -- TODO TestLabel "closing active vault works regardless of current dir" $

    ]

test_closeVault :: Test
test_closeVault = TestList [
    -- TODO closing *local* vault succeeds
    --  assert cwd becomes srcDir
    --  assert loopDev, mappedDev, repoDir are unreadable
    --  assert no active vault
    --  assert git log updated

    -- TODO closing *remote* vault succeeds
    --  assert cwd becomes srcDir
    --  assert loopDev, mappedDev, repoDir are unreadable
    --  assert no active vault
    --  assert git log *not* updated

    -- TODO closing fails
    ]
