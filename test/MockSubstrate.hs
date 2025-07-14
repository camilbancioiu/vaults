{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import Control.Exception.Base
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import System.Exit
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

data Mock = Mock
  { currentDir :: FilePath,
    prevDir :: FilePath,
    hasVaultDir :: Bool,
    hasRepoDir :: Bool,
    hasNVIMConfig :: Bool,
    envVars :: [(String, String)],
    multiVaultInfo :: [(FilePath, Base.VaultInfo)],
    nExecs :: Int,
    execRecorded :: [(String, [String])],
    execResults :: [Substrate.ExecResult],
    createdDirs :: [FilePath],
    existingDirs :: [FilePath],
    listingDirs :: [FilePath],
    writtenFiles :: [(FilePath, FilePath, String)],
    lastWrittenFile :: (FilePath, FilePath, String),
    callExceptions :: [Either String ()]
  }
  deriving (Show)

setCurrentDir ::
  String ->
  Mock ->
  Mock
setCurrentDir dir mock =
  mock
    { currentDir = dir,
      prevDir = previousMockDir
    }
  where
    previousMockDir = currentDir mock

addCreatedDir ::
  String ->
  Mock ->
  Mock
addCreatedDir dir mock =
  mock
    { createdDirs = dir : prevCreatedDirs
    }
  where
    prevCreatedDirs = createdDirs mock

addWrittenFile ::
  FilePath ->
  String ->
  Mock ->
  Mock
addWrittenFile fpath contents mock =
  mock
    { writtenFiles = prevWrittenFiles ++ [addedFile],
      lastWrittenFile = addedFile
    }
  where
    addedFile = (cwd, fpath, contents)
    prevWrittenFiles = writtenFiles mock
    cwd = currentDir mock

addMockEnvVar ::
  String ->
  String ->
  Mock ->
  Mock
addMockEnvVar key val mock =
  mock
    { envVars = newEnvVars
    }
  where
    newEnvVars = (key, val) : (envVars mock)

removeMockEnvVar ::
  String ->
  Mock ->
  Mock
removeMockEnvVar key mock =
  mock
    { envVars = newEnvVars
    }
  where
    newEnvVars = filter ((key /=) . fst) (envVars mock)

incExecs ::
  Mock ->
  Mock
incExecs mock =
  mock
    { nExecs = (nExecs mock) + 1
    }

recordExec ::
  (String, [String]) ->
  Mock ->
  Mock
recordExec execCom mock =
  mock
    { execRecorded = newExecRecorded
    }
  where
    newExecRecorded = (execRecorded mock) ++ [execCom]

addMockExecResult ::
  Substrate.ExecResult ->
  Mock ->
  Mock
addMockExecResult er mock =
  addMockExecResults [er] mock

addMockExecResults ::
  [Substrate.ExecResult] ->
  Mock ->
  Mock
addMockExecResults ers mock =
  mock
    { execResults = newExecResults
    }
  where
    newExecResults = (execResults mock) ++ ers

dropHeadMockExecResult ::
  Mock ->
  Mock
dropHeadMockExecResult mock =
  mock
    { execResults = tail (execResults mock)
    }

addMockExceptions ::
  [Either String ()] ->
  Mock ->
  Mock
addMockExceptions mexs mock =
  mock
    { callExceptions = (callExceptions mock) ++ mexs
    }

dropHeadMockExceptions ::
  Mock ->
  Mock
dropHeadMockExceptions mock =
  mock
    { callExceptions = tail (callExceptions mock)
    }

instance Substrate.Substrate (State Mock) where
  readFile = mock_readFile
  writeFile = mock_writeFile
  dirExists = mock_dirExists
  fileExists = mock_fileExists
  getDir = mock_getDir
  listDirs = mock_listDirs
  createDir = mock_createDir
  changeDir = mock_changeDir
  lookupEnv = mock_lookupEnv
  setEnv = mock_setEnv
  unsetEnv = mock_unsetEnv
  exec = mock_exec
  call = mock_call
  delay = mock_delay
  echo = mock_echo
  sync = mock_sync

-- TODO needs to read from writtenFiles as well?
mock_readFile ::
  FilePath ->
  State Mock String
mock_readFile fpath = do
  modify $ recordExec ("readFile", [fpath])
  vi <- getCurrentVaultInfo
  case fpath of
    ".vault/name" -> return (Base.name vi)
    ".vault/local" -> return (Base.localname vi)
    ".vault/remotes" -> return (unlines $ Base.remotes vi)
    ".vault/remoteStore" -> return (Base.remoteStore vi)
    _ -> return "not found"

getCurrentVaultInfo ::
  State Mock Base.VaultInfo
getCurrentVaultInfo = do
  cdir <- gets currentDir
  mvi <- gets multiVaultInfo
  case lookup cdir mvi of
    Nothing -> return mockVaultInfo
    Just vi -> return vi

mock_writeFile ::
  FilePath ->
  String ->
  State Mock ()
mock_writeFile fpath contents = do
  modify $ recordExec ("writeFile", [fpath])
  modify (addWrittenFile fpath contents)

mock_dirExists ::
  FilePath ->
  State Mock Bool
mock_dirExists dir = do
  modify $ recordExec ("dirExists", [dir])
  case dir of
    ".vault" -> gets hasVaultDir
    "repo" -> gets hasRepoDir
    _ -> do
      exdirs <- gets existingDirs
      cdirs <- gets createdDirs
      return $ elem dir (exdirs ++ cdirs)

mock_fileExists ::
  FilePath ->
  State Mock Bool
mock_fileExists fpath = do
  modify $ recordExec ("fileExists", [fpath])
  case fpath of
    "./.config/nvim/init.vim" -> gets hasNVIMConfig
    _ -> do
      wfiles <- gets writtenFiles
      let getfpath = \(_, fpath, _) -> fpath
      let fpaths = map getfpath wfiles
      return (elem fpath fpaths)

mock_getDir ::
  State Mock FilePath
mock_getDir = do
  modify $ recordExec ("getDir", [])
  gets currentDir

mock_listDirs ::
  State Mock [FilePath]
mock_listDirs = do
  modify $ recordExec ("listDirs", [])
  gets listingDirs

mock_createDir ::
  FilePath ->
  State Mock ()
mock_createDir dir = do
  modify $ recordExec ("createDir", [dir])
  modify $ addCreatedDir dir

mock_changeDir ::
  String ->
  State Mock ()
mock_changeDir dir = do
  modify $ recordExec ("changeDir", [dir])
  modify $ setCurrentDir dir

mock_lookupEnv ::
  String ->
  State Mock (Maybe String)
mock_lookupEnv key = do
  when
    (key /= "TEST")
    (modify $ recordExec ("lookupEnv", [key]))
  mock <- get
  return (lookup key $ envVars mock)

mock_setEnv ::
  String ->
  String ->
  State Mock ()
mock_setEnv key val = do
  when
    (key /= "TEST")
    (modify $ recordExec ("setEnv", [key]))
  modify (addMockEnvVar key val)

mock_unsetEnv ::
  String ->
  State Mock ()
mock_unsetEnv key = do
  modify $ recordExec ("unsetEnv", [key])
  modify (removeMockEnvVar key)

mock_exec ::
  String ->
  [String] ->
  String ->
  State Mock Substrate.ExecResult
mock_exec executable params _ = do
  modify $ recordExec (executable, params)
  modify incExecs
  ers <- gets $ execResults
  let er =
        if null ers
          then error ("empty execResults: " ++ executable ++ (show params))
          else head ers
  modify dropHeadMockExecResult
  return er

mock_call ::
  FilePath ->
  [String] ->
  State Mock (Either String ())
mock_call executable params = do
  modify $ recordExec (executable, params)
  modify incExecs
  mexcepts <- gets callExceptions
  if null mexcepts
    then return $ Right ()
    else do
      return $ head mexcepts

mock_delay ::
  Int ->
  State Mock ()
mock_delay _ = modify $ recordExec ("delay", [])

mock_echo ::
  String ->
  State Mock ()
mock_echo _ = return ()

mock_sync ::
  State Mock (Either String ())
mock_sync = do
  modify $ recordExec ("sync", [])
  return $ Right ()

mockVaultInfo =
  Base.VaultInfo
    { Base.name = "mockVault",
      Base.localname = "local",
      Base.remotes = ["remoteA", "remoteB"],
      Base.remoteStore = "ssh://remoteStore"
    }

mockVaultSourceDir = "/home/user/vaults/mockVault"

-- TODO update mountpoint format
mockVaultRuntimeInfo =
  Base.VaultRuntimeInfo
    { Base.srcDir = mockVaultSourceDir,
      Base.loopDev = "/dev/loop9",
      Base.mapperDev = "/dev/dm-2",
      Base.mountpoint = "/run/media/user/localhostname/mockVault",
      Base.partition = "local.vault",
      Base.partitionName = "local",
      Base.partitionLocation = Base.LocalPartition
    }

emptyMock =
  Mock
    { currentDir = mockVaultSourceDir,
      prevDir = "/",
      hasVaultDir = False,
      hasRepoDir = False,
      hasNVIMConfig = False,
      envVars = [],
      multiVaultInfo = [],
      nExecs = 0,
      execRecorded = [],
      execResults = [],
      createdDirs = [],
      existingDirs = [],
      listingDirs = [],
      writtenFiles = [],
      lastWrittenFile = ("", "", ""),
      callExceptions = []
    }

mockWithVaultDir =
  emptyMock
    { hasVaultDir = True
    }

mockWithVaultAndRepoDir =
  mockWithVaultDir
    { hasRepoDir = True
    }

mockMultiVault =
  emptyMock
    { currentDir = "vaults",
      existingDirs =
        [ "red",
          "red/.vault",
          "green",
          "green/.vault",
          "blue",
          "blue/.vault",
          "black",
          "white"
        ],
      listingDirs = ["red", "green", "blue", "black", "white"],
      multiVaultInfo =
        [ ("red", mockVaultInfo {Base.name = "red"}),
          ("blue", mockVaultInfo {Base.name = "blue"}),
          ("green", mockVaultInfo {Base.name = "green"})
        ]
    }
