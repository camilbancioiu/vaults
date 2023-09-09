module TestVaultOpen where

import Test.HUnit
import MockSubstrate

-- TODO test scenarios:
-- fail opening in non-vault folder
-- -- assert failure exit code
-- -- assert no calls to udisksctl
-- fail opening when any vault already open
-- -- assert failure exit code
-- -- assert no calls to udisksctl
--
-- fail when loop-setup fails
-- -- assert failure exit code
-- -- assert no other calls to udisksctl
-- fail when unlock fails
-- -- e.g. wrong passphrase
-- -- assert loop-delete called
-- -- assert no other calls to udisksctl
-- fail when mounting fails fails
-- -- assert lock called
-- -- assert loop-delete called
-- -- assert no other calls to udisksctl
--
-- succeed opening vault in correct conditions
-- -- assert cwd
-- -- assert active vault runtime info in env
-- succeed opening with partition name only
-- -- i.e. extension .vault added correctly
--
-- succeed forced opening in non-vault folder
-- fail forced opening when any vault already open

allTests :: Test
allTests = TestList [
    test_prerequisites
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "open in non-vault folder fails" $
    TestCase $ do
        let mock = emptyMock
        assertFailure "test incomplete"
    ]

test_loopSetupFails :: Test
test_loopSetupFails = TestCase $ do
    assertFailure "test not implemented"

emptyMock :: Mock
emptyMock = Mock {
    hasVaultDir = False,
    envVars = []
    }
