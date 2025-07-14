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
      let mock = addMockExecResults execResults mockWithVaultAndRepoDir
            where
              execResults =
                [ D.gitLogExec False,
                  D.unmountExec True,
                  D.lockExec True,
                  D.loopDeleteExec True
                ]
                  <*> (pure D.localOp2)
      let opResult = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
      let mockAfterExec = snd result
      let expectedCommands =
            [D.gitLogCmd]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp2)
      assertEqual
        "unmounted, locked, deleted loop"
        expectedCommands
        (execRecorded mockAfterExec)
      assertEqual
        "dir returned to srcDir"
        "/home/user/vaults/mockVault"
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
      let mock = addMockExecResults execResults mockWithVaultAndRepoDir
            where
              execResults =
                [ D.gitLogExec True,
                  D.unmountExec True,
                  D.lockExec True,
                  D.loopDeleteExec True
                ]
                  <*> (pure D.localOp2)
      let result = runState (runExceptT $ closeVault mockVaultRuntimeInfo) mock
      let mockAfterExec = snd result
      let expectedCommands =
            [D.gitLogCmd]
              ++ D.preClosePartitionCmds
              ++ (D.closePartitionCmds D.localOp2)
              ++ [("writeFile", ["local.log"])]
      assertEqual
        "unmounted, locked, deleted loop"
        expectedCommands
        (execRecorded mockAfterExec)
      assertEqual
        "dir changed to srcDir"
        "/home/user/vaults/mockVault"
        (currentDir mockAfterExec)
      assertEqual
        "git log saved"
        ("/home/user/vaults/mockVault", "local.log", D.commitLog D.localOp2)
        (lastWrittenFile mockAfterExec)
      assertAllExecsConsumed mockAfterExec

test_success_remote :: Test
test_success_remote =
  TestLabel "closing remote vault succeeds" $
    TestCase $ do
      let mock = addMockExecResults execResults mockWithVaultAndRepoDir
            where
              execResults =
                [ D.unmountExec True,
                  D.lockExec True,
                  D.loopDeleteExec True
                ]
                  <*> (pure D.localOp2)
      let mockRemoteVRI =
            mockVaultRuntimeInfo
              { partitionLocation = RemotePartition
              }
      let result = runState (runExceptT $ closeVault mockRemoteVRI) mock
      let mockAfterExec = snd result
      let expectedCommands =
            D.preClosePartitionCmds
              ++ D.closePartitionCmds D.localOp2
      assertEqual
        "unmounted, locked, deleted loop"
        expectedCommands
        (execRecorded mockAfterExec)
      assertEqual
        "dir changed to srcDir"
        "/home/user/vaults/mockVault"
        (currentDir mockAfterExec)
      assertEqual
        "git log not saved"
        (lastWrittenFile emptyMock)
        (lastWrittenFile mockAfterExec)
      assertAllExecsConsumed mockAfterExec
