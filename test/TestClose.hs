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

        let expectedCommands = [ D.gitLogCmd ]
                            ++   D.preClosePartitionCmds mockVaultRuntimeInfo
                            ++ ( D.closePartitionCmds D.localOp2 )
        assertEqual "unmounted, locked, deleted loop"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "dir returned to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log not saved"
            ("", "", "")
            (lastWrittenFile mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "closing vault succeeds" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.gitLogExec       True
                                   , D.unmountExec      True
                                   , D.lockExec         True
                                   , D.loopDeleteExec   True
                                   ] <*> (pure D.localOp2)
        let result = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
        let mockAfterExec = snd result

        let expectedCommands = [ D.gitLogCmd ]
                            ++   D.preClosePartitionCmds mockVaultRuntimeInfo
                            ++ ( D.closePartitionCmds D.localOp2 )
                            ++ [ ("writeFile", ["local.log"]) ]
        assertEqual "unmounted, locked, deleted loop"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log saved"
            ("/home/user/vaults/mockVault", "local.log", D.commitLog D.localOp2)
            (lastWrittenFile mockAfterExec)
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

        let expectedCommands = D.preClosePartitionCmds mockVaultRuntimeInfo
                            ++ D.closePartitionCmds D.localOp2
        assertEqual "unmounted, locked, deleted loop"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "dir changed to srcDir"
            "/home/user/vaults/mockVault"
            (currentDir mockAfterExec)
        assertEqual "git log not saved"
            (lastWrittenFile emptyMock)
            (lastWrittenFile mockAfterExec)
        assertAllExecsConsumed mockAfterExec

    ]
