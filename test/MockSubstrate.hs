{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import System.Exit
import Control.Monad.State

import qualified Vaults.Substrate as Substrate
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
    execResults :: [Substrate.ExecResult],
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

addMockExecResult :: Substrate.ExecResult -> Mock -> Mock
addMockExecResult er mock =
    addMockExecResults [er] mock

addMockExecResults :: [Substrate.ExecResult] -> Mock -> Mock
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

loopSetupOk   = Substrate.ExecResult ExitSuccess loopSetupOutput ""
loopSetupFail = Substrate.ExecResult (ExitFailure 16) "" "didnt work"
loopDeleteOk  = Substrate.ExecResult ExitSuccess "" ""
unlockOk      = Substrate.ExecResult ExitSuccess unlockOutput ""
unlockFail    = Substrate.ExecResult (ExitFailure 16) "" "didnt work"
mountOk       = Substrate.ExecResult ExitSuccess mountOutput ""
unmountOk     = Substrate.ExecResult ExitSuccess "" ""
mountFail     = Substrate.ExecResult (ExitFailure 16) "" "didnt work"
lockOk        = Substrate.ExecResult ExitSuccess lockOutput ""
gitLogOk      = Substrate.ExecResult ExitSuccess gitLogOutput ""
loopSetupOutput  = "Mapped file " ++ dummyPartition ++ " as " ++ dummyLoopDev ++ "."
unlockOutput     = "Unlocked " ++ dummyLoopDev ++ " as " ++ dummyMapperDev ++ "."
mountOutput      = "Mounted " ++ dummyMapperDev ++ " at " ++ dummyMountpoint
lockOutput       = "Locked " ++ dummyMapperDev ++ "."
gitLogOutput     = "38a3\nfb22\n8c2a\n02ad\n"

instance Substrate.Substrate (State Mock) where
    readFile  = mock_readFile
    writeFile = mock_writeFile
    dirExists = mock_dirExists
    lookupEnv = mock_lookupEnv
    setEnv    = mock_setEnv
    unsetEnv  = mock_unsetEnv
    getDir    = mock_getDir
    changeDir = mock_changeDir
    exec      = mock_exec

mock_readFile :: FilePath -> State Mock String
mock_readFile ".vault/name" = return (V.name mockVaultInfo)
mock_readFile ".vault/local" = return (V.localname mockVaultInfo)
mock_readFile ".vault/remotes" = return (unlines $ V.remotes mockVaultInfo)
mock_readFile ".vault/remoteStore" = return (V.remoteStore mockVaultInfo)

mock_writeFile :: FilePath -> String -> State Mock ()
mock_writeFile fpath contents =
    modify (addWrittenFile fpath contents)

mock_dirExists :: FilePath -> State Mock Bool
mock_dirExists ".vault" = gets hasVaultDir
mock_dirExists "repo" = gets hasRepoDir
mock_dirExists _ = return False

mock_lookupEnv :: String -> State Mock (Maybe String)
mock_lookupEnv key = do
    mock <- get
    return (lookup key $ envVars mock)

mock_setEnv :: String -> String -> State Mock ()
mock_setEnv key val = modify (addMockEnvVar key val)

mock_unsetEnv :: String -> State Mock ()
mock_unsetEnv key = modify (removeMockEnvVar key)

mock_getDir :: State Mock String
mock_getDir = gets currentDir

mock_changeDir :: String -> State Mock ()
mock_changeDir dir = modify (setCurrentDir dir)

mock_exec :: String -> [String] -> String -> State Mock Substrate.ExecResult
mock_exec executable params _ = do
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
