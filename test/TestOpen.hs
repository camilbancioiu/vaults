module TestOpen where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import System.Exit
import Test.HUnit
import qualified Vaults.Base as Base
import Vaults.Open
import qualified Vaults.Substrate as Substrate

-- TODO test scenarios:
-- succeed forced opening in non-vault folder

allTests :: Test
allTests =
  TestList
    [ test_openVault_without_partition_file,
      test_openVault_in_non_vault_folder,
      test_loopSetup_err_fails,
      test_unlock_fails_undoes_loopSetup,
      test_mount_fails_undoes_unlock_loopSetup,
      test_mount_ok_no_inner_repo,
      test_mount_ok_has_inner_repo
    ]

test_openVault_without_partition_file :: Test
test_openVault_without_partition_file =
  TestCase $ do
    let operation = openVault ""

    let mock = mockWithVaultDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult
    assertOpError "partition filename is required" operationResult
    assertNoExecCalls mockAfterExec

test_openVault_in_non_vault_folder :: Test
test_openVault_in_non_vault_folder =
  TestCase $ do
    let operation = openVault "local.vault"

    let mock = emptyMock
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult
    assertOpError "non-vault folder" operationResult
    assertNoExecCalls mockAfterExec

test_loopSetup_err_fails :: Test
test_loopSetup_err_fails =
  TestCase $ do
    let operation = openVault "local.vault"

    let mockExecResults = D.loopSetupExec False D.localOp

    let mock = addMockExecResult mockExecResults mockWithVaultDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    let failParams = snd $ D.loopSetupCmd D.localOp
    let failExec = D.loopSetupExec False D.localOp
    assertOpParamsError "loop-setup failed" failParams failExec operationResult

    let expectedOperationCmds = [D.loopSetupCmd D.localOp]
    let expectedCommands = D.preOpenPartitionCmds ++ expectedOperationCmds

    assertEqual
      "only loop-setup was called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertEqual
      "current directory not changed"
      (currentDir mockWithVaultDir)
      (currentDir mockAfterExec)
    assertAllExecsConsumed mockAfterExec

test_unlock_fails_undoes_loopSetup :: Test
test_unlock_fails_undoes_loopSetup =
  TestCase $ do
    let operation = openVault "local.vault"

    let mockExecResults =
          [ D.loopSetupExec True,
            D.unlockExec False,
            D.loopDeleteExec True
          ]
            <*> (pure D.localOp)

    let mock = addMockExecResults mockExecResults mockWithVaultDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    let failParams = snd $ D.unlockCmd D.localOp
    let failExec = D.unlockExec False D.localOp
    assertOpParamsError "unlock failed" failParams failExec operationResult

    let expectedOperationCmds =
          [ D.loopSetupCmd,
            D.unlockCmd,
            D.loopDeleteCmd
          ]
            <*> (pure D.localOp)
    let expectedCommands = D.preOpenPartitionCmds ++ expectedOperationCmds

    assertEqual
      "loop-setup, unlock, loop-delete were called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertEqual
      "current directory not changed"
      (currentDir mockWithVaultDir)
      (currentDir mockAfterExec)
    assertAllExecsConsumed mockAfterExec

test_mount_fails_undoes_unlock_loopSetup :: Test
test_mount_fails_undoes_unlock_loopSetup =
  TestCase $ do
    let operation = openVault "local.vault"

    let mockExecResults =
          [ D.loopSetupExec True,
            D.unlockExec True,
            D.mountExec False,
            D.lockExec True,
            D.loopDeleteExec True
          ]
            <*> (pure D.localOp)

    let mock = addMockExecResults mockExecResults mockWithVaultDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    let failParams = snd $ D.mountCmd D.localOp
    let failExec = D.mountExec False D.localOp
    assertOpParamsError "mount failed" failParams failExec operationResult

    let expectedOperationCmds =
          [ D.loopSetupCmd,
            D.unlockCmd,
            D.mountCmd,
            D.lockCmd,
            D.loopDeleteCmd
          ]
            <*> (pure D.localOp)
    let expectedCommands =
          D.preOpenPartitionCmds
            ++ expectedOperationCmds

    assertEqual
      "loop-setup, unlock, mount, lock, loop-delete were called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertEqual
      "current directory not changed"
      (currentDir mockWithVaultDir)
      (currentDir mockAfterExec)
    assertAllExecsConsumed mockAfterExec

test_mount_ok_no_inner_repo :: Test
test_mount_ok_no_inner_repo =
  TestCase $ do
    let operation = openVault "local.vault"

    let mockExecResults =
          [D.loopSetupExec, D.unlockExec, D.mountExec]
            <*> (pure True)
            <*> (pure D.localOp)

    let mock = addMockExecResults mockExecResults mockWithVaultDir

    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    let vri = D.makeVRI D.localOp False
    assertEqual "mount succeeds" (Right vri) (fst operationResult)

    let expectedCommands =
          D.preOpenPartitionCmds
            ++ (D.openPartitionCmds D.localOp)
            ++ D.postOpenPartitionCmds D.localOp

    assertEqual
      "loop-setup, unlock, mount, lock, loop-delete were called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertAllExecsConsumed mockAfterExec

test_mount_ok_has_inner_repo :: Test
test_mount_ok_has_inner_repo =
  TestCase $ do
    let operation = openVault "local.vault"
    let mockExecResults =
          [D.loopSetupExec, D.unlockExec, D.mountExec]
            <*> (pure True)
            <*> (pure D.localOp)
    let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir

    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    let vri = D.makeVRI D.localOp True
    assertEqual "mount succeeds" (Right vri) (fst operationResult)

    let expectedCommands =
          D.preOpenPartitionCmds
            ++ (D.openPartitionCmds D.localOp)
            ++ D.postOpenPartitionCmds D.localOp

    assertEqual
      "loop-setup, unlock, mount, lock, loop-delete were called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertAllExecsConsumed mockAfterExec
