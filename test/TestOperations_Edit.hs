module TestOperations_Edit where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.Operations as Operations

allTests :: Test
allTests =
  TestList
    [ test_editSuccessful,
      test_editorCrashes
    ]

test_editSuccessful :: Test
test_editSuccessful =
  TestLabel "edit successful" $
    TestCase $ do
      let operation = Operations.doEditVault mockVaultInfo
      let mock = addMockExecResults results mockWithVaultAndRepoDir
            where
              results =
                ( D.openPartitionExecOk
                    ++ [D.gitLogExec True]
                    ++ D.closePartitionExecOk
                )
                  <*> (pure D.localOp)
      let result = runState (runExceptT $ operation) mock
      let mockAfterExec = snd result
      assertEqual "vault opened, edited, closed" (Right ()) (fst result)

      let expectedCommands =
            D.preOpenPartitionCmds
              ++ (D.openPartitionCmds D.localOp)
              ++ D.postOpenPartitionCmds D.localOp
              ++ [D.changeToRepoDir D.localOp]
              ++ [ D.setEnvCmd "VIMRUNTIME",
                   D.editCmd D.localOp,
                   D.gitLogCmd
                 ]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp)
              ++ [("writeFile", ["local.log"])]

      assertEqual
        "all commands executed"
        expectedCommands
        (execRecorded mockAfterExec)
      assertAllExecsConsumed mockAfterExec

test_editorCrashes :: Test
test_editorCrashes =
  TestLabel "editor crashes" $
    TestCase $ do
      let operation = Operations.doEditVault mockVaultInfo
      let mock = addMockExecResults results mockWithVaultAndRepoDir
            where
              results =
                ( D.openPartitionExecOk
                    ++ [D.gitLogExec True]
                    ++ D.closePartitionExecOk
                )
                  <*> (pure D.localOp)
      let mockWithCrash =
            addMockExceptions
              [ Left "editor crashed"
              ]
              mock
      let result = runState (runExceptT $ operation) mockWithCrash
      let mockAfterExec = snd result
      assertEqual "vault opened, editor crashed, closed" (Left "editor crashed") (fst result)

      let expectedCommands =
            D.preOpenPartitionCmds
              ++ (D.openPartitionCmds D.localOp)
              ++ D.postOpenPartitionCmds D.localOp
              ++ [D.changeToRepoDir D.localOp]
              ++ [ D.setEnvCmd "VIMRUNTIME",
                   D.editCmd D.localOp,
                   D.gitLogCmd
                 ]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp)
              ++ [("writeFile", ["local.log"])]

      assertEqual
        "all commands executed"
        expectedCommands
        (execRecorded mockAfterExec)
      assertAllExecsConsumed mockAfterExec
