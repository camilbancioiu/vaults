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

        assertEqual "sync result successful" (Right()) (fst result)

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
