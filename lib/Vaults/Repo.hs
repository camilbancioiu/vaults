module Vaults.Repo where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Base as B
import qualified Vaults.Substrate as Substrate

data RepoIssue
  = UninitializedGit
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
  B.VaultInfo ->
  ExceptT RepoIssue m ()
verify vi = do
  checkGitInitialized
  checkRemotes vi
  checkSafeDirs vi

-- TODO rework handling the repo dir
requireRepoDir ::
  (Substrate.Substrate m) =>
  B.VaultRuntimeInfo ->
  ExceptT String m ()
requireRepoDir vri = do
  exists <- lift $ Substrate.dirExists "repo"
  if (not exists)
    then throwError "repo dir is required, but missing"
    else return ()

changeToRepoDir ::
  (Substrate.Substrate m) =>
  B.VaultRuntimeInfo ->
  ExceptT String m ()
changeToRepoDir vri = do
  case (B.repositoryDir vri) of
    Nothing -> throwError "repo dir is required, but missing"
    Just repoDir -> lift $ Substrate.changeDir repoDir

ensureRepoDir ::
  (Substrate.Substrate m) =>
  B.VaultRuntimeInfo ->
  ExceptT String m B.VaultRuntimeInfo
ensureRepoDir vri = do
  exists <- lift $ Substrate.dirExists "repo"
  if (not exists)
    then do
      lift $ Substrate.createDir "repo"
      let newVRI =
            vri
              { B.repositoryDir = Just $ (B.mountpoint vri) ++ "/repo"
              }
      return newVRI
    else return vri

checkGitInitialized ::
  (Substrate.Substrate m) =>
  ExceptT RepoIssue m ()
checkGitInitialized = do
  -- TODO move this call or delete it
  -- lift $ Substrate.changeDir "repo"
  result <- lift $ Substrate.exec "git" ["status"] ""
  let code = Substrate.exitCode result
  if code == ExitSuccess
    then return ()
    else
      if code == ExitFailure 128
        then throwError UninitializedGit
        else throwError (UnknownIssue (Substrate.errorOutput result))

-- TODO get the username via B.VaultInfo
checkRemotes ::
  (Substrate.Substrate m) =>
  B.VaultInfo ->
  ExceptT RepoIssue m ()
checkRemotes vi = do
  existingRemotes <- getRemotes
  user <- (withExceptT UnknownIssue) B.getUsername
  let expectedRemotes = makeExpectedRemotes (B.name vi) user (B.remotes vi)
  if existingRemotes == expectedRemotes
    then return ()
    else throwError (IncorrectGitRemotes expectedRemotes)

checkSafeDirs ::
  (Substrate.Substrate m) =>
  B.VaultInfo ->
  ExceptT RepoIssue m ()
checkSafeDirs vi = do
  user <- (withExceptT UnknownIssue) B.getUsername
  let expectedSafeDirs = makeExpectedSafeDirs (B.name vi) user (B.remotes vi)
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
