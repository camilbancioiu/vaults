module TestOperations where

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
      test_editSuccessful
    , test_syncSuccessful
    ]

-- TODO test where the editor crashes
test_editSuccessful :: Test
test_editSuccessful =
    TestLabel "edit successful" $
    TestCase $ do
        let operation = Operations.doEditVault mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = (  D.openPartitionExecOk
                                   ++ [ D.gitLogExec True ]
                                   ++ D.closePartitionExecOk
                                   ) <*> (pure D.localOp)
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result
        assertEqual "vault opened, edited, closed" (Right()) (fst result)

        -- No need to assert on a call to cd (cd needs a shell anyway); working
        -- dir is changed via Substrate.changeDir.
        let expectedCommands = (  D.openPartitionCmds
                               ++ [ D.editCmd, D.gitLogCmd ]
                               ++ D.closePartitionCmds
                               ) <*> (pure D.localOp)

        assertEqual "all commands executed"
            expectedCommands
            (execRecorded mockAfterExec)

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
