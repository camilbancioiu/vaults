module TestClose where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import Vaults.Base
import Vaults.Close

-- TODO closing fails

allTests :: Test
allTests =
  TestList
    [ test_success,
      test_success_remote,
      test_commitLogFails
    ]

test_commitLogFails :: Test
test_commitLogFails =
  TestLabel "exporting commit log fails; closing vault succeeds" $
    TestCase $ do
      let operation = closeVault mockVaultRuntimeInfo

      let expectedCommands =
            [D.gitLogCmd]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp2)

      let mockExecResults =
            [ D.gitLogExec False,
              D.unmountExec True,
              D.lockExec True,
              D.loopDeleteExec True
            ]
              <*> (pure D.localOp2)

      let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "unmounted, locked, deleted loop"
        expectedCommands
        (execRecorded mockAfterExec)

      assertEqual
        "dir returned to srcDir"
        mockVaultSourceDir
        (currentDir mockAfterExec)

      assertEqual
        "git log not saved"
        ("", "", "")
        (lastWrittenFile mockAfterExec)

      assertAllExecsConsumed mockAfterExec

test_success :: Test
test_success =
  TestLabel "closing vault succeeds" $
    TestCase $ do
      let operation = closeVault mockVaultRuntimeInfo

      let expectedCommands =
            [D.gitLogCmd]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp2)
              ++ [("writeFile", ["local.log"])]

      let mockExecResults =
            [ D.gitLogExec True,
              D.unmountExec True,
              D.lockExec True,
              D.loopDeleteExec True
            ]
              <*> (pure D.localOp2)

      let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "unmounted, locked, deleted loop"
        expectedCommands
        (execRecorded mockAfterExec)

      assertEqual
        "dir changed to srcDir"
        mockVaultSourceDir
        (currentDir mockAfterExec)

      let commitLogWithNL = (D.commitLog D.localOp2) ++ ['\n']
      let expectedFile = (mockVaultSourceDir, "local.log", commitLogWithNL)
      assertEqual
        "git log saved"
        expectedFile
        (lastWrittenFile mockAfterExec)

      assertAllExecsConsumed mockAfterExec

test_success_remote :: Test
test_success_remote =
  TestLabel "closing remote vault succeeds" $
    TestCase $ do
      let operation = closeVault D.mockRemoteVRI

      let expectedCommands =
            D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp2)

      let mockExecResults =
            [ D.unmountExec True,
              D.lockExec True,
              D.loopDeleteExec True
            ]
              <*> (pure D.localOp2)

      let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "unmounted, locked, deleted loop"
        expectedCommands
        (execRecorded mockAfterExec)

      assertEqual
        "dir changed to srcDir"
        mockVaultSourceDir
        (currentDir mockAfterExec)

      assertEqual
        "git log not saved"
        (lastWrittenFile emptyMock)
        (lastWrittenFile mockAfterExec)

      assertAllExecsConsumed mockAfterExec
