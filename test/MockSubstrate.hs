{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import System.Exit
import Control.Monad.State
import Vaults.Substrate
import qualified Vaults.Base as V

import Debug.Trace

data Mock = Mock {
    currentDir :: FilePath,
    prevDir :: FilePath,
    hasVaultDir :: Bool,
    hasRepoDir :: Bool,
    envVars :: [(String, String)],
    nExecs :: Int,
    execRecorded :: [(String, [String])],
    execResults :: [ExecResult],
    writtenFile :: (FilePath, FilePath, String)
} deriving Show

setCurrentDir :: String -> Mock -> Mock
setCurrentDir dir mock =
    mock {
        currentDir = dir,
        prevDir = previousMockDir
    }
    where previousMockDir = currentDir mock

addWrittenFile :: FilePath -> String -> Mock -> Mock
addWrittenFile fpath contents mock =
    mock {
        writtenFile = (cwd, fpath, contents)
    }
    where cwd = currentDir mock

addMockEnvVar :: String -> String -> Mock -> Mock
addMockEnvVar key val mock =
    mock {
        envVars = newEnvVars
    }
    where newEnvVars = (key, val):(envVars mock)

removeMockEnvVar :: String -> Mock -> Mock
removeMockEnvVar key mock =
    mock {
        envVars = newEnvVars
    }
    where newEnvVars = filter ((key /=) . fst) (envVars mock)

incExecs :: Mock -> Mock
incExecs mock =
    mock {
        nExecs = (nExecs mock) + 1
    }

recordExec :: (String, [String]) -> Mock -> Mock
recordExec execCom mock =
    mock {
        execRecorded = newExecRecorded
    }
    where newExecRecorded = (execRecorded mock) ++ [execCom]

addMockExecResult :: ExecResult -> Mock -> Mock
addMockExecResult er mock =
    addMockExecResults [er] mock

addMockExecResults :: [ExecResult] -> Mock -> Mock
addMockExecResults ers mock =
    mock {
        execResults = newExecResults
    }
    where newExecResults = (execResults mock) ++ ers

dropHeadMockExecResult :: Mock -> Mock
dropHeadMockExecResult mock =
    mock {
        execResults = tail (execResults mock)
    }

dummyPartition = "local.vault"
dummyLoopDev = "/dev/loop42"
dummyMapperDev = "/dev/dm-4"
dummyMountpoint = "/mnt/point"

loopSetupOk   = ExecResult ExitSuccess loopSetupOutput ""
loopSetupFail = ExecResult (ExitFailure 16) "" "didnt work"
loopDeleteOk  = ExecResult ExitSuccess "" ""
unlockOk      = ExecResult ExitSuccess unlockOutput ""
unlockFail    = ExecResult (ExitFailure 16) "" "didnt work"
mountOk       = ExecResult ExitSuccess mountOutput ""
unmountOk     = ExecResult ExitSuccess "" ""
mountFail     = ExecResult (ExitFailure 16) "" "didnt work"
lockOk        = ExecResult ExitSuccess lockOutput ""
gitLogOk      = ExecResult ExitSuccess gitLogOutput ""
loopSetupOutput  = "Mapped file " ++ dummyPartition ++ " as " ++ dummyLoopDev ++ "."
unlockOutput     = "Unlocked " ++ dummyLoopDev ++ " as " ++ dummyMapperDev ++ "."
mountOutput      = "Mounted " ++ dummyMapperDev ++ " at " ++ dummyMountpoint
lockOutput       = "Locked " ++ dummyMapperDev ++ "."
gitLogOutput     = "38a3\nfb22\n8c2a\n02ad\n"

instance Substrate (State Mock) where
    readFileSub  = mock_readFileSub
    writeFileSub = mock_writeFileSub
    dirExistsSub = mock_dirExistsSub
    lookupEnvSub = mock_lookupEnvSub
    setEnvSub    = mock_setEnvSub
    unsetEnvSub  = mock_unsetEnvSub
    getDirSub    = mock_getDirSub
    changeDirSub = mock_changeDirSub
    execSub      = mock_execSub

mock_readFileSub :: FilePath -> State Mock String
mock_readFileSub ".vault/name" = return (V.name mockVaultInfo)
mock_readFileSub ".vault/local" = return (V.localname mockVaultInfo)
mock_readFileSub ".vault/remotes" = return (unlines $ V.remotes mockVaultInfo)
mock_readFileSub ".vault/remoteStore" = return (V.remoteStore mockVaultInfo)

mock_writeFileSub :: FilePath -> String -> State Mock ()
mock_writeFileSub fpath contents =
    modify (addWrittenFile fpath contents)

mock_dirExistsSub :: FilePath -> State Mock Bool
mock_dirExistsSub ".vault" = gets hasVaultDir
mock_dirExistsSub "repo" = gets hasRepoDir
mock_dirExistsSub _ = return False

mock_lookupEnvSub :: String -> State Mock (Maybe String)
mock_lookupEnvSub key = do
    mock <- get
    return (lookup key $ envVars mock)

mock_setEnvSub :: String -> String -> State Mock ()
mock_setEnvSub key val = modify (addMockEnvVar key val)

mock_unsetEnvSub :: String -> State Mock ()
mock_unsetEnvSub key = modify (removeMockEnvVar key)

mock_getDirSub :: State Mock String
mock_getDirSub = gets currentDir

mock_changeDirSub :: String -> State Mock ()
mock_changeDirSub dir = modify (setCurrentDir dir)

mock_execSub :: String -> [String] -> String -> State Mock ExecResult
mock_execSub executable params _ = do
    modify $ recordExec (executable, params)
    modify incExecs
    er <- gets $ head . execResults
    modify dropHeadMockExecResult
    return er

mockVaultInfo = V.VaultInfo {
    V.name = "mockVault",
    V.localname = "local",
    V.remotes = ["remoteA", "remoteB"],
    V.remoteStore = "ssh://remoteStore"
}

mockVaultRuntimeInfo = V.VaultRuntimeInfo {
    V.srcDir = "/home/user/vaults/mockVault",
    V.loopDev = "/dev/loop9",
    V.mapperDev = "/dev/dm-2",
    V.mountpoint = "/run/media/user/localhostname/mockVault",
    V.repositoryDir = "/run/media/user/localhostname/mockVault/repo",
    V.partition = "local.vault",
    V.partitionName = "local",
    V.partitionLocation = V.LocalPartition
}

emptyMock :: Mock
emptyMock = Mock {
      currentDir = "/home/user"
    , prevDir = "/"
    , hasVaultDir = False
    , hasRepoDir = False
    , envVars = []
    , nExecs = 0
    , execRecorded = []
    , execResults = []
    , writtenFile = ("", "", "")
    }

mockWithVaultDir :: Mock
mockWithVaultDir = emptyMock {
    hasVaultDir = True
    }

mockWithVaultAndRepoDir :: Mock
mockWithVaultAndRepoDir = emptyMock {
      hasVaultDir = True
    , hasRepoDir = True
}

mockWithActiveVault :: Mock
mockWithActiveVault = mockWithVaultAndRepoDir {
    envVars = [(V.activeVaultEnvName, show mockVaultRuntimeInfo)]
}

mockWithEnvVar :: (String, String) -> Mock
mockWithEnvVar var = emptyMock {
    envVars = [var]
}
