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
  | IncorrectGitRemotes [GitRemote]
  | IncorrectGitSafeDirs [FilePath]
  | UnknownIssue String
  deriving (Eq, Show)

data GitRemote = GitRemote
  { remoteName :: String,
    remoteURL :: FilePath
  }
  deriving (Eq, Show)

verify ::
  (Substrate.Substrate m) =>
  Base.VaultInfo ->
  ExceptT RepoIssue m ()
verify vi = do
  checkRepoDir
  checkGitInitialized
  checkRemotes vi
  checkSafeDirs vi

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
  let code = Substrate.exitCode result
  if code == ExitSuccess
    then return ()
    else
      if code == ExitFailure 128
        then throwError UninitializedGit
        else throwError (UnknownIssue (Substrate.errorOutput result))

-- TODO get the username via Base.VaultInfo
checkRemotes ::
  (Substrate.Substrate m) =>
  Base.VaultInfo ->
  ExceptT RepoIssue m ()
checkRemotes vi = do
  existingRemotes <- getRemotes
  user <- (withExceptT UnknownIssue) Base.getUsername
  let expectedRemotes = makeExpectedRemotes (Base.name vi) user (Base.remotes vi)
  if existingRemotes == expectedRemotes
    then return ()
    else throwError (IncorrectGitRemotes expectedRemotes)

checkSafeDirs ::
  (Substrate.Substrate m) =>
  Base.VaultInfo ->
  ExceptT RepoIssue m ()
checkSafeDirs vi = do
  user <- (withExceptT UnknownIssue) Base.getUsername
  let expectedSafeDirs = makeExpectedSafeDirs (Base.name vi) user (Base.remotes vi)
  existingSafeDirs <- getExistingSafeDirs
  if existingSafeDirs == expectedSafeDirs
    then return ()
    else throwError (IncorrectGitSafeDirs expectedSafeDirs)

makeExpectedSafeDirs ::
  String ->
  String ->
  [String] ->
  [FilePath]
makeExpectedSafeDirs vaultName user remotes =
  map (++ "/.git") expectedRemoteURLs
  where
    expectedRemotes = makeExpectedRemotes vaultName user remotes
    expectedRemoteURLs = map remoteURL expectedRemotes

-- TODO refactor duplicate code of calling git commands with output handling
getExistingSafeDirs ::
  (Substrate.Substrate m) =>
  ExceptT RepoIssue m [FilePath]
getExistingSafeDirs = do
  -- The relevant safe.directory entries are those recorded in local
  -- configuration. Vaults don't manage the global git config entries.
  result <- lift $ Substrate.exec "git" ["config", "get", "--local", "--all"] ""
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError (UnknownIssue (Substrate.errorOutput result)))
  return $ lines (Substrate.output result)

makeExpectedRemotes :: String -> String -> [String] -> [GitRemote]
makeExpectedRemotes vaultName user remotes =
  map (makeRemoteByName vaultName user) remotes

makeRemoteByName :: String -> String -> String -> GitRemote
makeRemoteByName vaultName user remoteName =
  GitRemote remoteName url
  where
    url = makeRemoteURL vaultName user remoteName

makeRemoteURL ::
  String ->
  String ->
  String ->
  FilePath
makeRemoteURL vaultName user remoteName =
  "/usr/media/" ++ user ++ "/" ++ fsLabel ++ "/repo"
  where
    fsLabel = vaultName ++ "-" ++ remoteName

getCurrentBranch ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getCurrentBranch = do
  result <- lift $ Substrate.exec "git" ["branch", "--show-current"] ""
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError "could not get current branch")
  let currentBranch = Substrate.output result
  return currentBranch

getRemotes ::
  (Substrate.Substrate m) =>
  ExceptT RepoIssue m [GitRemote]
getRemotes = do
  result <- lift $ Substrate.exec "git" ["remote", "--verbose"] ""
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError (UnknownIssue (Substrate.errorOutput result)))
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
