module TestOpen where

import Control.Monad.State
import Control.Monad.Except
import System.Exit

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

import Vaults.Open

-- TODO test scenarios:
-- succeed opening with partition name only
-- -- i.e. extension .vault added correctly
--
-- succeed forced opening in non-vault folder
-- fail forced opening when any vault already open

allTests :: Test
allTests = TestList [
      test_prerequisites
    , test_openVault
    ]

test_prerequisites :: Test
test_prerequisites = TestList [
    TestLabel "open in non-vault folder fails" $
    TestCase $ do
        let mock = emptyMock
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result
        assertOpError "non-vault folder" result
        assertNoExecCalls mockAfterExec,

    TestLabel "open without a partition filename fails" $
    TestCase $ do
        let mock = mockWithVaultDir
        let result = runState (runExceptT $ openVault "") mock
        let mockAfterExec = snd result
        assertOpError "partition filename is required" result
        assertNoExecCalls mockAfterExec
    ]

test_openVault :: Test
test_openVault = TestList [
    TestLabel "loop-setup error prevents opening" $
    TestCase $ do
        let mock = addMockExecResult loopSetupFail mockWithVaultDir
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let failParams = snd $ D.loopSetupCmd D.localOp
        assertOpParamsError "loop-setup failed" failParams loopSetupFail result

        let expectedCommands = [ D.loopSetupCmd D.localOp ]
        assertEqual "only loop-setup was called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "unlock error prevents opening and deletes loop device" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockFail, loopDeleteOk]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let failParams = snd $ D.unlockCmd D.localOp
        assertOpParamsError "unlock failed" failParams unlockFail result

        let expectedCommands = ([ D.loopSetupCmd
                                , D.unlockCmd
                                , D.loopDeleteCmd
                                ] <*> (pure D.localOp))
        assertEqual "loop-setup, unlock, loop-delete were called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "mount error prevents opening and undoes unlock and loop-setup" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockOk, mountFail, lockOk, loopDeleteOk]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let failParams = snd $ D.mountCmd D.localOp
        assertOpParamsError "mount failed" failParams mountFail result

        let expectedCommands = ([ D.loopSetupCmd
                                , D.unlockCmd
                                , D.mountCmd
                                , D.lockCmd
                                , D.loopDeleteCmd
                                ] <*> (pure D.localOp))
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "mount succeeds, no inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockOk, mountOk]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let dummyVRI = makeDummyVRI ""
        assertEqual "mount succeeds" (Right dummyVRI) (fst result)

        let expectedCommands = (D.openPartitionCmds <*> (pure D.localOp))
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "mount succeeds, vault has inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [loopSetupOk, unlockOk, mountOk]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let vri = makeDummyVRI "/repo"
        assertEqual "mount succeeds" (Right vri) (fst result)

        let expectedCommands = (D.openPartitionCmds <*> (pure D.localOp))
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

    ]

makeDummyVRI :: FilePath -> Base.VaultRuntimeInfo
makeDummyVRI repoDir = Base.VaultRuntimeInfo {
                  Base.srcDir = "/home/user",
                  Base.loopDev = "/dev/loop42",
                  Base.mapperDev = "/dev/dm-4",
                  Base.mountpoint = "/mnt/point",
                  Base.repositoryDir = "/mnt/point" ++ repoDir,
                  Base.partition = "local.vault",
                  Base.partitionName = "local",
                  Base.partitionLocation = Base.LocalPartition
              }
