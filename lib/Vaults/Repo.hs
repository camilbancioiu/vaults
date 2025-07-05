module Vaults.Repo where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Base as Base
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

verifyRepo ::
  (Substrate.Substrate m) =>
  Base.VaultInfo ->
  ExceptT RepoIssue m ()
verifyRepo vi = do
  checkRepoDir
  checkGitInitialized
  checkRemotes vi

checkRepoDir ::
  (Substrate.Substrate m) =>
  ExceptT RepoIssue m ()
checkRepoDir = do
  exists <- lift $ Substrate.dirExists "repo"
  if (not exists)
    then throwError MissingRepoDir
    else return ()

checkGitInitialized ::
  (Substrate.Substrate m) =>
  ExceptT RepoIssue m ()
checkGitInitialized = do
  result <- lift $ Substrate.exec "git" ["status"] ""
  if (Substrate.exitCode result) /= ExitSuccess
    then throwError UninitializedGit
    else return ()

checkRemotes ::
  (Substrate.Substrate m) =>
  Base.VaultInfo ->
  ExceptT RepoIssue m ()
checkRemotes vi = do
  existingRemotes <- runExceptT $ getRemotes
  return ()

getCurrentBranch ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getCurrentBranch = do
  result <- lift $ Substrate.exec "git" ["branch", "--show-current"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get current branch")
  let currentBranch = Substrate.output result
  return currentBranch

getRemotes ::
  (Substrate.Substrate m) =>
  ExceptT String m [GitRemote]
getRemotes = do
  result <- lift $ Substrate.exec "git" ["remote", "--verbose"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get remotes")
  let remotesOutput = Substrate.output result
  let parsedRemotes = parseGitRemotes remotesOutput
  return parsedRemotes

parseGitRemotes :: String -> [GitRemote]
parseGitRemotes gitOut =
  map parseGitRemote (lines gitOut)

parseGitRemote :: String -> GitRemote
parseGitRemote line =
  GitRemote name url
  where
    (name, tab : url) = break (== '\t') line
