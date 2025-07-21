module Vaults.Repo where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Base as B
import Vaults.Substrate2 (Substrate)
import qualified Vaults.Substrate2 as Substrate

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

makeConformant ::
  (Substrate m) =>
  B.VaultInfo ->
  ExceptT String m ()
makeConformant vi = do
  -- TODO get the username via B.VaultInfo
  gitInitialized <- checkGitInitialized
  unless gitInitialized callGitInit
  removeGitRemotes
  user <- B.getUsername
  configureGitRemotes vi user
  eraseGitSafeDirs
  configureGitSafeDirs vi user

verify ::
  (Substrate m) =>
  B.VaultInfo ->
  ExceptT RepoIssue m ()
verify vi = do
  gitInitialized <- withExceptT UnknownIssue checkGitInitialized
  unless gitInitialized (throwError UninitializedGit)
  checkRemotes vi
  checkSafeDirs vi

callGitInit ::
  (Substrate m) =>
  ExceptT String m ()
callGitInit = Substrate.call "git" ["init"]

removeGitRemotes ::
  (Substrate m) =>
  ExceptT String m ()
removeGitRemotes = do
  remoteNames <- withExceptT show getRemoteNames
  mapM_ removeGitRemote remoteNames

removeGitRemote ::
  (Substrate m) =>
  String ->
  ExceptT String m ()
removeGitRemote name = do
  Substrate.call
    "git"
    ["remote", "remove", name]
  Substrate.echo ("removed remote: " ++ name)

configureGitRemotes ::
  (Substrate m) =>
  B.VaultInfo ->
  String ->
  ExceptT String m ()
configureGitRemotes vi user = do
  let vaultName = B.name vi
  let names = B.remotes vi
  let expectedRemotes = makeExpectedRemotes vaultName user names
  mapM_ addGitRemote expectedRemotes

addGitRemote ::
  (Substrate m) =>
  GitRemote ->
  ExceptT String m ()
addGitRemote remote = do
  Substrate.call
    "git"
    [ "remote",
      "add",
      (remoteName remote),
      (remoteURL remote)
    ]
  Substrate.echo ("added remote: " ++ (show remote))

eraseGitSafeDirs ::
  (Substrate m) =>
  ExceptT String m ()
eraseGitSafeDirs =
  catchError eraseCmd skipIfError
  where
    eraseCmd =
      do
        Substrate.call
          "git"
          ["config", "unset", "--local", "--all", "safe.directory"]
        Substrate.echo
          "removed local safe.dir entries"

    skipIfError =
      ( \_ -> do
          Substrate.echo "no existing safe.dirs, skipping..."
          return ()
      )

configureGitSafeDirs ::
  (Substrate m) =>
  B.VaultInfo ->
  String ->
  ExceptT String m ()
configureGitSafeDirs vi user = do
  let vaultName = B.name vi
  let names = B.remotes vi
  let expectedRemotes = makeExpectedRemotes vaultName user names
  mapM_ configureGitSafeDir expectedRemotes

configureGitSafeDir ::
  (Substrate m) =>
  GitRemote ->
  ExceptT String m ()
configureGitSafeDir remote =
  do
    Substrate.call
      "git"
      [ "config",
        "set",
        "--local",
        "--append",
        "safe.directory",
        (remoteURL remote) ++ "/.git"
      ]
    Substrate.echo $
      "added safe.directory: "
        ++ (remoteURL remote)
        ++ "/.git"

-- TODO rework handling the repo dir

-- TODO add test when repo dir is missing, to see the thrown error
changeToRepoDir ::
  (Substrate m) =>
  B.VaultRuntimeInfo ->
  ExceptT String m ()
changeToRepoDir vri =
  catchError
    (Substrate.changeDir B.repoDirName)
    (\_ -> throwError "repo dir is required, but missing")

ensureRepoDir ::
  (Substrate m) =>
  B.VaultRuntimeInfo ->
  ExceptT String m ()
ensureRepoDir vri = do
  exists <- Substrate.dirExists B.repoDirName
  if (not exists)
    then Substrate.createDir B.repoDirName
    else return ()

checkGitInitialized ::
  (Substrate m) =>
  ExceptT String m Bool
checkGitInitialized = do
  result <- Substrate.exec "git" ["status"] ""
  let code = Substrate.exitCode result
  if code == ExitSuccess
    then return True
    else
      if code == ExitFailure 128
        then return False
        else throwError (Substrate.errorOutput result)

-- TODO get the username via B.VaultInfo
checkRemotes ::
  (Substrate m) =>
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
  (Substrate m) =>
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
  (Substrate m) =>
  ExceptT RepoIssue m [FilePath]
getExistingSafeDirs = do
  -- The relevant safe.directory entries are those recorded in local
  -- configuration. Vaults don't manage the global git config entries.
  let execOperation = Substrate.exec "git" ["config", "get", "--local", "--all", "safe.directory"] ""
  result <- withExceptT UnknownIssue execOperation
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError (UnknownIssue (Substrate.errorOutput result)))
  return $ lines (Substrate.output result)

makeExpectedRemotes :: String -> String -> [String] -> [GitRemote]
makeExpectedRemotes vaultName user remotes =
  map (makeRemoteByName vaultName user) remotes

makeRemoteByName :: String -> String -> String -> GitRemote
makeRemoteByName vaultName user name =
  GitRemote name url
  where
    url = makeRemoteURL vaultName user name

makeRemoteURL ::
  String ->
  String ->
  String ->
  FilePath
makeRemoteURL vaultName user name =
  "/run/media/" ++ user ++ "/" ++ fsLabel ++ "/repo"
  where
    fsLabel = vaultName ++ "-" ++ name

getCurrentBranch ::
  (Substrate m) =>
  ExceptT String m String
getCurrentBranch = do
  result <- Substrate.exec "git" ["branch", "--show-current"] ""
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError "could not get current branch")
  let currentBranch = Substrate.output result
  return currentBranch

getRemotes ::
  (Substrate m) =>
  ExceptT RepoIssue m [GitRemote]
getRemotes = do
  let execOperation = Substrate.exec "git" ["remote", "--verbose"] ""
  result <- withExceptT UnknownIssue execOperation
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError (UnknownIssue (Substrate.errorOutput result)))
  let remotesOutput = Substrate.output result
  let parsedRemotes = parseGitRemotes remotesOutput
  return parsedRemotes

getRemoteNames ::
  (Substrate m) =>
  ExceptT String m [String]
getRemoteNames = do
  result <- Substrate.exec "git" ["remote"] ""
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError (Substrate.errorOutput result))
  let remotesOutput = Substrate.output result
  return $ lines remotesOutput

parseGitRemotes :: String -> [GitRemote]
parseGitRemotes gitOut =
  map parseGitRemote (lines gitOut)

parseGitRemote :: String -> GitRemote
parseGitRemote line =
  GitRemote name url
  where
    (name, tab : url) = break (== '\t') line
