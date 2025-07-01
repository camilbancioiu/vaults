module Vaults.Repo where

import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Substrate as Substrate

data RepoIssue
  = MissingRepoDir
  | UninitializedGit
  | MissingGitRemotes [GitRemote]
  | MissingGitSafeDirs [GitRemote]
  deriving (Eq, Show)

data GitRemote = GitRemote
  { remoteName :: String,
    remoteURL :: String
  }
  deriving (Eq, Show)

verifyRepo :: (Substrate.Substrate m) => ExceptT RepoIssue m ()
verifyRepo = do
  checkRepoDir
  checkGitInitialized

checkRepoDir :: (Substrate.Substrate m) => ExceptT RepoIssue m ()
checkRepoDir = do
  exists <- lift $ Substrate.dirExists "repo"
  if (not exists)
    then throwError MissingRepoDir
    else return ()

checkGitInitialized :: (Substrate.Substrate m) => ExceptT RepoIssue m ()
checkGitInitialized = do
  result <- lift $ Substrate.exec "git" ["status"] ""
  if (Substrate.exitCode result) /= ExitSuccess
    then throwError UninitializedGit
    else return ()
