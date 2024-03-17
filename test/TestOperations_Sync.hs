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
    ]

test_syncSuccessful :: Test
test_syncSuccessful =
    TestLabel "sync successful" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
        -- TODO There is no `git fetch` in the execResults below because `git
        -- fetch` is called with Substrate.call, not Substrate.exec. A
        -- different test is requried to assert on errors with `git fetch`.
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results =  openRemote
                                 ++ openLocal
                                 ++ gitLog
                                 ++ closeLocal
                                 ++ closeRemote
                         openRemote  = D.openPartitionExecOk <*> (pure D.remoteOp)
                         openLocal   = D.openPartitionExecOk <*> (pure D.localOp)
                         gitLog      = [ D.gitLogExec True D.localOp ]
                         closeLocal  = D.closePartitionExecOk <*> (pure D.localOp)
                         closeRemote = D.closePartitionExecOk <*> (pure D.remoteOp)
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        assertEqual "sync successful" (Right ()) (fst result)

        let expectedCommands = ( openRemote
                              ++ openLocal
                              ++ gitFetch
                              ++ gitLog
                              ++ closeLocal
                              ++ closeRemote
                               )
                               where openRemote  = D.openPartitionCmds <*> (pure D.remoteOp)
                                     openLocal   = D.openPartitionCmds <*> (pure D.localOp)
                                     gitFetch    = [ D.gitFetchCmd "remoteA" D.localOp ]
                                     gitLog      = [ D.gitLogCmd D.localOp ]
                                     closeLocal  = D.closePartitionCmds <*> (pure D.localOp)
                                     closeRemote = D.closePartitionCmds <*> (pure D.remoteOp)

        assertEqual "vaults opened, local fetched remoteA, vaults closed"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

-- TODO test_sync_LocalFailed_GitLog
-- TODO test_sync_LocalFailed_Unmount
-- TODO test_sync_LocalFailed_Lock
-- TODO test_sync_LocalFailed_LoopDelete
--
-- TODO test_sync_RemoteFailed_Unmount
-- TODO test_sync_RemoteFailed_Lock
-- TODO test_sync_RemoteFailed_LoopDelete

test_sync_RemoteFailed_LoopSetup :: Test
test_sync_RemoteFailed_LoopSetup =
    TestLabel "remote loop-setup failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec False D.remoteOp ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.loopSetupCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = [ D.loopSetupCmd D.remoteOp ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_RemoteFailed_Unlock :: Test
test_sync_RemoteFailed_Unlock =
    TestLabel "remote unlock failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec  True  D.remoteOp
                                   , D.unlockExec     False D.remoteOp
                                   , D.loopDeleteExec True  D.remoteOp
                                   ]
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result

        let expectedError = Left $ D.showFailedCmd (D.unlockCmd D.remoteOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = [ D.loopSetupCmd  D.remoteOp
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
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
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

        let expectedCommands = [ D.loopSetupCmd  D.remoteOp
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
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
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

        let expectedCommands = [ D.loopSetupCmd  D.remoteOp
                               , D.unlockCmd     D.remoteOp
                               , D.mountCmd      D.remoteOp
                               , D.loopSetupCmd  D.localOp
                               , D.unmountCmd    D.remoteOp
                               , D.lockCmd       D.remoteOp
                               , D.loopDeleteCmd D.remoteOp
                               ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_Unlock :: Test
test_sync_LocalFailed_Unlock =
    TestLabel "unlock local failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
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

        let expectedCommands = [ D.loopSetupCmd  D.remoteOp
                               , D.unlockCmd     D.remoteOp
                               , D.mountCmd      D.remoteOp
                               , D.loopSetupCmd  D.localOp
                               , D.unlockCmd     D.localOp
                               , D.loopDeleteCmd D.localOp
                               , D.unmountCmd    D.remoteOp
                               , D.lockCmd       D.remoteOp
                               , D.loopDeleteCmd D.remoteOp
                               ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_Mount :: Test
test_sync_LocalFailed_Mount =
    TestLabel "unlock local failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
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

        let expectedCommands = [ D.loopSetupCmd  D.remoteOp
                               , D.unlockCmd     D.remoteOp
                               , D.mountCmd      D.remoteOp
                               , D.loopSetupCmd  D.localOp
                               , D.unlockCmd     D.localOp
                               , D.mountCmd      D.localOp
                               , D.lockCmd       D.localOp
                               , D.loopDeleteCmd D.localOp
                               , D.unmountCmd    D.remoteOp
                               , D.lockCmd       D.remoteOp
                               , D.loopDeleteCmd D.remoteOp
                               ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

test_sync_LocalFailed_GitLog :: Test
test_sync_LocalFailed_GitLog =
    TestLabel "git-log local failure handled" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
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

        let expectedError = Left $ D.showFailedCmd (D.gitLogCmd D.localOp)
        assertEqual "sync failed" expectedError (fst result)

        let expectedCommands = [ D.loopSetupCmd  D.remoteOp
                               , D.unlockCmd     D.remoteOp
                               , D.mountCmd      D.remoteOp
                               , D.loopSetupCmd  D.localOp
                               , D.unlockCmd     D.localOp
                               , D.mountCmd      D.localOp
                               , D.gitFetchCmd   "remoteA" D.localOp
                               , D.gitLogCmd     D.localOp
                               , D.unmountCmd    D.localOp
                               , D.lockCmd       D.localOp
                               , D.loopDeleteCmd D.localOp
                               , D.unmountCmd    D.remoteOp
                               , D.lockCmd       D.remoteOp
                               , D.loopDeleteCmd D.remoteOp
                               ]
        assertEqual "commands"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec
