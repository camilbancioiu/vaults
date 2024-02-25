module TestOpCloseVault where

import Control.Monad.State
import Control.Monad.Except

import Test.HUnit
import Assertions
import MockSubstrate

import Vaults.OpCloseVault

allTests :: Test
allTests = TestList [
      test_prerequisites
    , test_closeVault
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "closing fails when no active vault" $
    TestCase $ do
        let mock = emptyMock
        let result = runState (runExceptT $ closeVault) mock
        let mockAfterExec = snd result
        assertOpError "cannot read vault runtime info" result
        assertNoExecCalls mockAfterExec

    -- TODO TestLabel "closing active vault works regardless of current dir" $

    ]

test_closeVault :: Test
test_closeVault = TestList [
    -- TODO closing *local* vault succeeds
    --  assert git log updated

    -- TODO closing *remote* vault succeeds
    --  assert cwd becomes srcDir
    --  assert loopDev, mappedDev, repoDir are unreadable
    --  assert no active vault
    --  assert git log *not* updated

    -- TODO closing fails

    TestLabel "closing vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithActiveVault
                   where results = [gitLogOk, unmountOk, lockOk, loopDeleteOk]
        let result = runState (runExceptT $ closeVault) mock
        let mockAfterExec = snd result
        assertNoVaultEnvVar mockAfterExec
        assertEqual "unmounted, locked, deleted loop"
            [ ("git", ["log", "--format=%H"])
            , ("udisksctl", ["unmount", "-b", "/dev/dm-2"])
            , ("udisksctl", ["lock", "-b", "/dev/dm-2"])
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop9"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log saved"
            ("/home/user/vaults/mockVault", "local.log", gitLogOutput)
            (writtenFile mockAfterExec)

    ]

