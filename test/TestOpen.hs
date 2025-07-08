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
      -- test_mount_ok_has_inner_repo_verified
    ]

test_openVault_without_partition_file :: Test
test_openVault_without_partition_file =
  TestCase $ do
    let mock = mockWithVaultDir
    let result = runState (runExceptT $ openVault "") mock
    let mockAfterExec = snd result
    assertOpError "partition filename is required" result
    assertNoExecCalls mockAfterExec

test_openVault_in_non_vault_folder :: Test
test_openVault_in_non_vault_folder =
  TestCase $ do
    let mock = emptyMock
    let result = runState (runExceptT $ openVault "local.vault") mock
    let mockAfterExec = snd result
    assertOpError "non-vault folder" result
    assertNoExecCalls mockAfterExec

test_loopSetup_err_fails :: Test
test_loopSetup_err_fails =
  TestCase $ do
    let mock = addMockExecResult result mockWithVaultDir
          where
            result = D.loopSetupExec False D.localOp
    let result = runState (runExceptT $ openVault "local.vault") mock
    let mockAfterExec = snd result

    let failParams = snd $ D.loopSetupCmd D.localOp
    let failExec = D.loopSetupExec False D.localOp
    assertOpParamsError "loop-setup failed" failParams failExec result

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
  TestCase $
    do
      let mock = addMockExecResults results mockWithVaultDir
            where
              results =
                [ D.loopSetupExec True,
                  D.unlockExec False,
                  D.loopDeleteExec True
                ]
                  <*> (pure D.localOp)
      let result = runState (runExceptT $ openVault "local.vault") mock
      let mockAfterExec = snd result

      let failParams = snd $ D.unlockCmd D.localOp
      let failExec = D.unlockExec False D.localOp
      assertOpParamsError "unlock failed" failParams failExec result

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
    let mock = addMockExecResults results mockWithVaultDir
          where
            results =
              [ D.loopSetupExec True,
                D.unlockExec True,
                D.mountExec False,
                D.lockExec True,
                D.loopDeleteExec True
              ]
                <*> (pure D.localOp)
    let result = runState (runExceptT $ openVault "local.vault") mock
    let mockAfterExec = snd result

    let failParams = snd $ D.mountCmd D.localOp
    let failExec = D.mountExec False D.localOp
    assertOpParamsError "mount failed" failParams failExec result

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
    let mock = addMockExecResults results mockWithVaultDir
          where
            results = udisksExecResults
            udisksExecResults =
              [D.loopSetupExec, D.unlockExec, D.mountExec]
                <*> (pure True)
                <*> (pure D.localOp)
    let result = runState (runExceptT $ openVault "local.vault") mock
    let mockAfterExec = snd result

    let vri = D.makeVRI D.localOp ""
    assertEqual "mount succeeds" (Right vri) (fst result)

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
    let mock = addMockExecResults results mockWithVaultAndRepoDir
          where
            results = udisksExecResults
            udisksExecResults =
              [D.loopSetupExec, D.unlockExec, D.mountExec]
                <*> (pure True)
                <*> (pure D.localOp)
    let result = runState (runExceptT $ openVault "local.vault") mock
    let mockAfterExec = snd result

    let vri = D.makeVRI D.localOp "/repo"
    assertEqual "mount succeeds" (Right vri) (fst result)

    let expectedCommands =
          D.preOpenPartitionCmds
            ++ (D.openPartitionCmds D.localOp)
            ++ D.postOpenPartitionCmds D.localOp

    assertEqual
      "loop-setup, unlock, mount, lock, loop-delete were called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertAllExecsConsumed mockAfterExec

test_mount_ok_has_inner_repo_verified :: Test
test_mount_ok_has_inner_repo_verified =
  TestCase $ do
    let mockWithER = addMockExecResults results mockWithVaultAndRepoDir
          where
            results = udisksExecResults ++ D.successfulRepoVerificationExecResults
            udisksExecResults =
              [D.loopSetupExec, D.unlockExec, D.mountExec]
                <*> (pure True)
                <*> (pure D.localOp)
    let mock = addMockEnvVar "TEST" "VERIFY" mockWithER
    let result = runState (runExceptT $ openVault "local.vault") mock
    let mockAfterExec = snd result

    let vri = D.makeVRI D.localOp "/repo"
    assertEqual "mount succeeds" (Right vri) (fst result)

    let expectedCommands =
          D.preOpenPartitionCmds
            ++ (D.openPartitionCmds D.localOp)
            ++ D.postOpenPartitionCmds D.localOp
            ++ D.verifyRepoCmds

    assertEqual
      "loop-setup, unlock, mount, lock, loop-delete were called"
      expectedCommands
      (execRecorded mockAfterExec)
    assertAllExecsConsumed mockAfterExec
