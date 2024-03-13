module TestOpen where

import Control.Monad.State
import Control.Monad.Except
import System.Exit

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

import Test.HUnit
import Assertions
import MockSubstrate

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
        let failParams = ["loop-setup", "-f", "local.vault"]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result
        assertOpParamsError "loop-setup failed" failParams loopSetupFail result
        assertEqual "only loop-setup was called"
            [("udisksctl", failParams)]
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec),

    TestLabel "unlock error prevents opening and deletes loop device" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockFail, loopDeleteOk]
        let failParams = ["unlock", "-b", "/dev/loop42"]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result
        assertOpParamsError "unlock failed" failParams unlockFail result
        assertEqual "loop-setup, unlock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", failParams)
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop42"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec),

    TestLabel "mount error prevents opening and undoes unlock and loop-setup" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockOk, mountFail, lockOk, loopDeleteOk]
        let failParams = ["mount", "-b", "/dev/dm-4"]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result
        assertOpParamsError "mount failed" failParams mountFail result
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", failParams)
            , ("udisksctl", ["lock", "-b", "/dev/dm-4"])
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop42"])
            ]
            (execRecorded mockAfterExec)
        assertEqual "current directory not changed"
            "/home/user"
            (currentDir mockAfterExec),

    TestLabel "mount succeeds, no inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultDir
                   where results = [loopSetupOk, unlockOk, mountOk]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result
        let dummyVRI = makeDummyVRI ""
        assertEqual "mount succeeds" (Right dummyVRI) (fst result)
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["mount", "-b", "/dev/dm-4"])
            ]
            (execRecorded mockAfterExec),

    TestLabel "mount succeeds, vault has inner repo" $
    TestCase $ do
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = [loopSetupOk, unlockOk, mountOk]
        let result = runState (runExceptT $ openVault "local.vault") mock
        let mockAfterExec = snd result
        let vri = makeDummyVRI "/repo"
        assertEqual "mount succeeds" (Right vri) (fst result)
        assertEqual "loop-setup, unlock, mount, lock, loop-delete were called"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["mount", "-b", "/dev/dm-4"])
            ]
            (execRecorded mockAfterExec)

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
