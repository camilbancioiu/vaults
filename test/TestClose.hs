module TestClose where

import Control.Monad.State
import Control.Monad.Except

import Test.HUnit
import Assertions
import MockSubstrate

import Vaults.Close

allTests :: Test
allTests = TestList [
    test_closeVault
    ]

test_closeVault :: Test
test_closeVault = TestList [
    -- TODO closing *local* vault succeeds
    --  assert git log updated

    -- TODO closing *remote* vault succeeds
    --  assert cwd becomes srcDir
    --  assert loopDev, mappedDev, repoDir are unreadable
    --  assert git log *not* updated

    -- TODO closing fails

    TestLabel "exporting commit log fails; closing vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [gitLogFail, unmountOk, lockOk, loopDeleteOk]
        let result = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
        let mockAfterExec = snd result
        assertEqual "unmounted, locked, deleted loop"
            [ ("git", ["log", "--format=%H"])
            , ("udisksctl", ["unmount", "-b", "/dev/dm-2"])
            , ("udisksctl", ["lock", "-b", "/dev/loop9"])
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop9"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log not saved"
            ("", "", "")
            (writtenFile mockAfterExec),

    TestLabel "closing vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [gitLogOk, unmountOk, lockOk, loopDeleteOk]
        let result = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
        let mockAfterExec = snd result
        assertEqual "unmounted, locked, deleted loop"
            [ ("git", ["log", "--format=%H"])
            , ("udisksctl", ["unmount", "-b", "/dev/dm-2"])
            , ("udisksctl", ["lock", "-b", "/dev/loop9"])
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

