module TestOperations_Sync where

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Operations as Operations

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

allTests :: Test
allTests = TestList [
      test_syncSuccessful
    , test_sync_RemoteFailed_LoopSetup
    , test_sync_RemoteFailed_Unlock
    , test_sync_RemoteFailed_Mount
    , test_sync_LocalFailed_LoopSetup
    , test_sync_LocalFailed_Unlock
    , test_sync_LocalFailed_Mount
    , test_sync_LocalFailed_GitLog
    , test_sync_LocalFailed_Unmount
    , test_sync_LocalFailed_Lock
    , test_sync_LocalFailed_LoopDelete
    , test_sync_RemoteFailed_Unmount
    , test_sync_RemoteFailed_Lock
    , test_sync_RemoteFailed_LoopDelete
    ]

test_syncSuccessful :: Test
test_syncSuccessful =
    TestLabel "sync successful" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        -- TODO There is no `git fetch` in the execResults below because `git
        -- fetch` is called with Substrate.call, not Substrate.exec. A
        -- different test is requried to assert on errors with `git fetch`.
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec True  D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        assertEqual "sync successful" (Right ()) (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.localOp
                          ++ [("writeFile", ["local.log"])]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_RemoteFailed_LoopSetup :: Test
test_sync_RemoteFailed_LoopSetup =
    TestLabel "remote loop-setup failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec False D.remoteOp ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.loopSetupCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd D.remoteOp ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_RemoteFailed_Unlock :: Test
test_sync_RemoteFailed_Unlock =
    TestLabel "remote unlock failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     False D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.unlockCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.loopDeleteCmd D.remoteOp
                             ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_RemoteFailed_Mount :: Test
test_sync_RemoteFailed_Mount =
    TestLabel "remote mount failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      False D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.mountCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             , D.lockCmd       D.remoteOp
                             , D.loopDeleteCmd D.remoteOp
                             ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_LoopSetup :: Test
test_sync_LocalFailed_LoopSetup =
    TestLabel "local loop-setup failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  False D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.loopSetupCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_Unlock :: Test
test_sync_LocalFailed_Unlock =
    TestLabel "local unlock failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     False D.localOp
                                   , D.loopDeleteExec True  D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.unlockCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                               , D.unlockCmd     D.remoteOp
                               , D.mountCmd      D.remoteOp
                               ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                               , D.unlockCmd     D.localOp
                               , D.loopDeleteCmd D.localOp
                               ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_Mount :: Test
test_sync_LocalFailed_Mount =
    TestLabel "local mount failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      False D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec True  D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.mountCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                               , D.unlockCmd     D.remoteOp
                               , D.mountCmd      D.remoteOp
                               ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                               , D.unlockCmd     D.localOp
                               , D.mountCmd      D.localOp
                               , D.lockCmd       D.localOp
                               , D.loopDeleteCmd D.localOp
                               ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_GitLog :: Test
test_sync_LocalFailed_GitLog =
    TestLabel "git-log local failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     False D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec True  D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd D.gitLogCmd
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.localOp
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_Unmount :: Test
test_sync_LocalFailed_Unmount =
    TestLabel "local unmount failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    False  D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.unmountCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++ [ D.unmountCmd    D.localOp
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_Lock :: Test
test_sync_LocalFailed_Lock =
    TestLabel "local lock failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       False D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.lockCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++ [ D.unmountCmd    D.localOp
                             , D.delayCmd
                             , D.lockCmd       D.localOp
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec


test_sync_LocalFailed_LoopDelete :: Test
test_sync_LocalFailed_LoopDelete =
    TestLabel "local loop-delete failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec False D.remoteOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.loopDeleteCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.localOp
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_RemoteFailed_Unmount :: Test
test_sync_RemoteFailed_Unmount =
    TestLabel "remote unmount failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec True  D.localOp
                                   , D.unmountExec    False D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.unmountCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.localOp
                          ++ [("writeFile", ["local.log"])]
                          ++   D.preClosePartitionCmds
                          ++ [ D.unmountCmd    D.remoteOp
                             ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_RemoteFailed_Lock :: Test
test_sync_RemoteFailed_Lock =
    TestLabel "remote lock failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec True  D.localOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       False D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.lockCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.localOp
                          ++ [("writeFile", ["local.log"])]
                          ++   D.preClosePartitionCmds
                          ++ [ D.unmountCmd    D.remoteOp
                             , D.delayCmd
                             , D.lockCmd       D.remoteOp
                             ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec


test_sync_RemoteFailed_LoopDelete :: Test
test_sync_RemoteFailed_LoopDelete =
    TestLabel "remote loop-delete failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault "remoteA" mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     True  D.remoteOp
                                   , D.mountExec      True  D.remoteOp
                                   , D.loopSetupExec  True  D.localOp
                                   , D.unlockExec     True  D.localOp
                                   , D.mountExec      True  D.localOp
                                   , D.gitLogExec     True  D.localOp
                                   , D.unmountExec    True  D.localOp
                                   , D.lockExec       True  D.localOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   , D.unmountExec    True  D.remoteOp
                                   , D.lockExec       True  D.remoteOp
                                   , D.loopDeleteExec False  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.loopDeleteCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.remoteOp
                             , D.unlockCmd     D.remoteOp
                             , D.mountCmd      D.remoteOp
                             ]
                          ++   D.postOpenPartitionCmds D.remoteOp
                          ++ [ D.changeToSrcDir ]
                          ++   D.preOpenPartitionCmds
                          ++ [ D.loopSetupCmd  D.localOp
                             , D.unlockCmd     D.localOp
                             , D.mountCmd      D.localOp
                             ]
                          ++   D.postOpenPartitionCmds D.localOp
                          ++ [ D.changeToRepoDir D.localOp ]
                          ++ [ D.gitFetchCmd   "remoteA" D.localOp
                             , D.gitLogCmd
                             ]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.localOp
                          ++ [("writeFile", ["local.log"])]
                          ++   D.preClosePartitionCmds
                          ++   D.closePartitionCmds D.remoteOp
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec
