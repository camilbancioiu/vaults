{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import System.Exit
import Control.Monad.State
import Control.Exception.Base

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

data Mock = Mock {
      currentDir :: FilePath
    , prevDir :: FilePath
    , hasVaultDir :: Bool
    , hasRepoDir :: Bool
    , hasNVIMConfig :: Bool
    , envVars :: [(String, String)]
    , nExecs :: Int
    , execRecorded :: [(String, [String])]
    , execResults :: [Substrate.ExecResult]
    , createdDirs :: [String]
    , writtenFiles :: [(FilePath, FilePath, String)]
    , lastWrittenFile :: (FilePath, FilePath, String)
    , callExceptions :: [Maybe MockException]
} deriving Show

data MockException = MockException String
                   deriving (Show, Eq)

instance Exception MockException

setCurrentDir :: String -> Mock -> Mock
setCurrentDir dir mock =
    mock {
        currentDir = dir,
        prevDir = previousMockDir
    }
    where previousMockDir = currentDir mock

addCreatedDir :: String -> Mock ->  Mock
addCreatedDir dir mock =
    mock {
        createdDirs = dir:prevCreatedDirs
    }
    where prevCreatedDirs = createdDirs mock

addWrittenFile :: FilePath -> String -> Mock -> Mock
addWrittenFile fpath contents mock =
    mock {
          writtenFiles = prevWrittenFiles ++ [addedFile]
        , lastWrittenFile = addedFile
    }
    where addedFile = (cwd, fpath, contents)
          prevWrittenFiles = writtenFiles mock
          cwd = currentDir mock

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

addMockExceptions :: [Maybe String] -> Mock -> Mock
addMockExceptions mexs mock =
    mock {
        callExceptions = (callExceptions mock) ++ newCallExceptions
    }
    where newCallExceptions = map (fmap MockException) mexs

dropHeadMockExceptions :: Mock -> Mock
dropHeadMockExceptions mock =
    mock {
        callExceptions = tail (callExceptions mock)
    }

instance Substrate.Substrate (State Mock) where
    readFile   = mock_readFile
    writeFile  = mock_writeFile
    dirExists  = mock_dirExists
    fileExists = mock_fileExists
    getDir     = mock_getDir
    createDir  = mock_createDir
    changeDir  = mock_changeDir
    lookupEnv  = mock_lookupEnv
    setEnv     = mock_setEnv
    unsetEnv   = mock_unsetEnv
    exec       = mock_exec
    call       = mock_call
    delay      = mock_delay
    echo       = mock_echo

mock_fileExists :: FilePath -> State Mock Bool
mock_fileExists "./.config/nvim/init.vim" = gets hasNVIMConfig
mock_fileExists _ = return False

-- TODO replace mockVaultInfo with a member of Mock
-- TODO which can be configured by the calling test
mock_readFile :: FilePath -> State Mock String
mock_readFile ".vault/name" = return (Base.name mockVaultInfo)
mock_readFile ".vault/local" = return (Base.localname mockVaultInfo)
mock_readFile ".vault/remotes" = return (unlines $ Base.remotes mockVaultInfo)
mock_readFile ".vault/remoteStore" = return (Base.remoteStore mockVaultInfo)

mock_writeFile :: FilePath -> String -> State Mock ()
mock_writeFile fpath contents =
    modify (addWrittenFile fpath contents)

mock_dirExists :: FilePath -> State Mock Bool
mock_dirExists ".vault" = gets hasVaultDir
mock_dirExists "repo" = gets hasRepoDir
mock_dirExists _ = return False

mock_getDir :: State Mock String
mock_getDir = gets currentDir

mock_createDir :: FilePath -> State Mock ()
mock_createDir dir = modify $ addCreatedDir dir

mock_changeDir :: String -> State Mock ()
mock_changeDir dir = modify $ setCurrentDir dir

mock_lookupEnv :: String -> State Mock (Maybe String)
mock_lookupEnv key = do
    mock <- get
    return (lookup key $ envVars mock)

mock_setEnv :: String -> String -> State Mock ()
mock_setEnv key val = modify (addMockEnvVar key val)

mock_unsetEnv :: String -> State Mock ()
mock_unsetEnv key = modify (removeMockEnvVar key)

mock_exec :: String -> [String] -> String -> State Mock Substrate.ExecResult
mock_exec executable params _ = do
    modify $ recordExec (executable, params)
    modify incExecs
    er <- gets $ head . execResults
    modify dropHeadMockExecResult
    return er

mock_call :: FilePath -> [String] -> State Mock ()
mock_call executable params = do
    modify $ recordExec (executable, params)
    modify incExecs
    mexcepts <- gets callExceptions
    if null mexcepts
       then return ()
       else do
           let mexcept = head mexcepts
           modify dropHeadMockExceptions
           case mexcept of
                Nothing -> return ()
                Just ex -> throw ex

mock_delay :: Int -> State Mock ()
mock_delay _ = return ()

mock_echo :: String -> State Mock ()
mock_echo _ = return ()

mockVaultInfo = Base.VaultInfo {
    Base.name = "mockVault",
    Base.localname = "local",
    Base.remotes = ["remoteA", "remoteB"],
    Base.remoteStore = "ssh://remoteStore"
}

mockVaultRuntimeInfo = Base.VaultRuntimeInfo {
    Base.srcDir = "/home/user/vaults/mockVault",
    Base.loopDev = "/dev/loop9",
    Base.mapperDev = "/dev/dm-2",
    Base.mountpoint = "/run/media/user/localhostname/mockVault",
    Base.repositoryDir = "/run/media/user/localhostname/mockVault/repo",
    Base.partition = "local.vault",
    Base.partitionName = "local",
    Base.partitionLocation = Base.LocalPartition
}

emptyMock :: Mock
emptyMock = Mock {
      currentDir = "/home/user"
    , prevDir = "/"
    , hasVaultDir = False
    , hasRepoDir = False
    , hasNVIMConfig = False
    , envVars = []
    , nExecs = 0
    , execRecorded = []
    , execResults = []
    , createdDirs = []
    , writtenFiles = []
    , lastWrittenFile = ("", "", "")
    , callExceptions = []
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

mockWithEnvVar :: (String, String) -> Mock
mockWithEnvVar var = emptyMock {
    envVars = [var]
}
