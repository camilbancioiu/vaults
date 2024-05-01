module TestOperations_Edit where

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Operations as Operations

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

-- TODO test with shell
-- TODO test where the shell crashes

allTests :: Test
allTests = TestList [
    test_editSuccessful,
    test_editorCrashes
    ]

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
        assertAllExecsConsumed mockAfterExec

test_editorCrashes :: Test
test_editorCrashes =
    TestLabel "editor crashes" $
    TestCase $ do
        let operation = Operations.doEditVault mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = (  D.openPartitionExecOk
                                   ++ [ D.gitLogExec True ]
                                   ++ D.closePartitionExecOk
                                   ) <*> (pure D.localOp)
        let mockWithCrash = addMockExceptions [ Left "editor crashed"
                                              ] mock
        let result = runState (runExceptT $ operation) mockWithCrash
        let mockAfterExec = snd result
        assertEqual "vault opened, editor crashed, closed" (Left "editor crashed") (fst result)

        let expectedCommands = (  D.openPartitionCmds
                               ++ [ D.editCmd, D.gitLogCmd ]
                               ++ D.closePartitionCmds
                               ) <*> (pure D.localOp)

        assertEqual "all commands executed"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec
