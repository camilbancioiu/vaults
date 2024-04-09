module TestClose where

import Control.Monad.State
import Control.Monad.Except

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

import Vaults.Base
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
    --  assert loopDev, mapperDev, repoDir are unreadable
    --  assert git log *not* updated

    -- TODO closing fails

    TestLabel "exporting commit log fails; closing vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.gitLogExec       False
                                   , D.unmountExec      True
                                   , D.lockExec         True
                                   , D.loopDeleteExec   True
                                   ] <*> (pure D.localOp2)
        let result = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
        let mockAfterExec = snd result

        let expectedCommands = D.closePartitionWithLogCmds <*> (pure D.localOp2)
        assertEqual "unmounted, locked, deleted loop"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log not saved"
            ("", "", "")
            (writtenFile mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "closing vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.gitLogExec       False
                                   , D.unmountExec      True
                                   , D.lockExec         True
                                   , D.loopDeleteExec   True
                                   ] <*> (pure D.localOp2)
        let result = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
        let mockAfterExec = snd result

        let expectedCommands = D.closePartitionWithLogCmds <*> (pure D.localOp2)
        assertEqual "unmounted, locked, deleted loop"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log saved"
            ("/home/user/vaults/mockVault", "local.log", D.commitLog D.localOp)
            (writtenFile mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "closing remote vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.unmountExec      True
                                   , D.lockExec         True
                                   , D.loopDeleteExec   True
                                   ] <*> (pure D.localOp2)
        let mockRemoteVRI = mockVaultRuntimeInfo {
            partitionLocation = RemotePartition
            }
        let result = runState (runExceptT $ closeVault mockRemoteVRI) mock
        let mockAfterExec = snd result

        let expectedCommands = D.closePartitionCmds <*> (pure D.localOp2)
        assertEqual "unmounted, locked, deleted loop"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertAllExecsConsumed mockAfterExec

    ]
