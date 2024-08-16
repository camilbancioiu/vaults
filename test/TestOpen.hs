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
-- succeed forced opening in non-vault folder

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
        let mock = addMockExecResult result mockWithVaultDir
                   where result = D.loopSetupExec False D.localOp
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let failParams = snd $ D.loopSetupCmd D.localOp
        let failExec = D.loopSetupExec False D.localOp
        assertOpParamsError "loop-setup failed" failParams failExec result

        let expectedOperationCmds = [ D.loopSetupCmd D.localOp ]
        let expectedCommands = D.preOpenPartitionCmds ++ expectedOperationCmds

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
                   where results = [ D.loopSetupExec True
                                   , D.unlockExec    False
                                   , D.loopDeleteExec  True
                                   ] <*> (pure D.localOp)
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let failParams = snd $ D.unlockCmd D.localOp
        let failExec = D.unlockExec False D.localOp
        assertOpParamsError "unlock failed" failParams failExec result

        let expectedOperationCmds = [ D.loopSetupCmd
                                    , D.unlockCmd
                                    , D.loopDeleteCmd
                                    ] <*> (pure D.localOp)
        let expectedCommands = D.preOpenPartitionCmds ++ expectedOperationCmds

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
                   where results = [ D.loopSetupExec  True
                                   , D.unlockExec     True
                                   , D.mountExec      False
                                   , D.lockExec       True
                                   , D.loopDeleteExec True
                                   ] <*> (pure D.localOp)
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let failParams = snd $ D.mountCmd D.localOp
        let failExec = D.mountExec False D.localOp
        assertOpParamsError "mount failed" failParams failExec result

        let expectedOperationCmds = [ D.loopSetupCmd
                                    , D.unlockCmd
                                    , D.mountCmd
                                    , D.lockCmd
                                    , D.loopDeleteCmd
                                    ] <*> (pure D.localOp)
        let expectedCommands = D.preOpenPartitionCmds
                            ++ expectedOperationCmds

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
                   where results = [ D.loopSetupExec True
                                   , D.unlockExec    True
                                   , D.mountExec     True
                                   ] <*> (pure D.localOp)
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let vri = D.makeVRI D.localOp ""
        assertEqual "mount succeeds" (Right vri) (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ ( D.openPartitionCmds D.localOp )
                          ++   D.postOpenPartitionCmds

        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec,

    TestLabel "mount succeeds, vault has inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [ D.loopSetupExec True
                                   , D.unlockExec    True
                                   , D.mountExec     True
                                   ] <*> (pure D.localOp)
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result

        let vri = D.makeVRI D.localOp "/repo"
        assertEqual "mount succeeds" (Right vri) (fst result)

        let expectedCommands = D.preOpenPartitionCmds
                          ++ ( D.openPartitionCmds D.localOp )
                          ++ [ ("changeDir", []),       -- TODO why?
                               ("dirExists", ["repo"])
                             ]

        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            expectedCommands
            (execRecorded mockAfterExec)
        assertAllExecsConsumed mockAfterExec

    ]
