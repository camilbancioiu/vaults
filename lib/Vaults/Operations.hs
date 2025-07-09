module Vaults.Operations where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.List
import System.Exit
import Vaults.Base
import Vaults.Close
import qualified Vaults.CustomCfg as Cfg
import Vaults.Init
import Vaults.MkPartition
import Vaults.Open
import qualified Vaults.Repo as Repo
import qualified Vaults.Substrate as Substrate

data Operation
  = InitVault String String
  | MakePartition String Int
  | SetupVault
  | EditVault
  | ShellVault
  | ShellPartition String
  | UploadVault
  | UploadMultiVault
  | DownloadVault
  | DownloadMultiVault
  | SyncVault String
  | SyncEditVault String
  | DiffLog
  | DiffLogMultiVault
  deriving (Show)

doInitVault ::
  (Substrate.Substrate m) =>
  String ->
  String ->
  ExceptT String m ()
doInitVault vaultName localName = do
  initVault vaultName localName

doMakePartition ::
  (Substrate.Substrate m) =>
  String ->
  Int ->
  VaultInfo ->
  ExceptT String m ()
doMakePartition = makePartition

doSetupVault ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  ExceptT String m ()
doSetupVault vi = do
  vri <- openVault $ (localname vi) ++ ".vault" -- TODO refactor into f:openLocalPartition
  lift $ Substrate.echo "Vault opened, performing verifications..."
  runVerification vi vri
  closeVault vri
  lift $ Substrate.echo "Vault closed."

runVerification ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  VaultRuntimeInfo ->
  ExceptT String m ()
runVerification vi vri = do
  let verification = (withExceptT (show) (Repo.verify vi))
  catchError verification (lift . Substrate.echo)
  return ()

doEditVault ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  ExceptT String m ()
doEditVault vi = do
  vri <- openVault $ (localname vi) ++ ".vault"
  lift $ Substrate.echo "Vault opened, starting editor..."
  editVault vri
  closeVault vri
  lift $ Substrate.echo "Vault closed."

editVault ::
  (Substrate.Substrate m) =>
  VaultRuntimeInfo ->
  ExceptT String m ()
editVault vri = do
  ( do
      lift $ Substrate.changeDir (repositoryDir vri)
      callEditor
    )
    `catchError` (\e -> closeVault vri >> throwError e)
  lift $ Substrate.echo "Editor closed."

-- TODO replace Cfg.defaultEditCfg with actual cfg read from the opened vault
callEditor ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
callEditor = do
  let cfg = Cfg.defaultEditCfg
  let envvars = Cfg.envVars cfg
  let editorExec = Cfg.editor cfg
  let editorParams = Cfg.editorCLIParams cfg
  setEditingEnvVars envvars
  ExceptT $ Substrate.call editorExec editorParams

setEditingEnvVars ::
  (Substrate.Substrate m) =>
  [(String, String)] ->
  ExceptT String m ()
setEditingEnvVars (var : vars) = do
  let envkey = fst var
  let envvalue = snd var
  lift $ Substrate.setEnv envkey envvalue
  unless (null vars) (setEditingEnvVars vars)

-- TODO write extra tests (see test/TestOperations_Shell.hs)
doShellVault ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  ExceptT String m ()
doShellVault vi = do
  vri <- openVault $ (localname vi) ++ ".vault"
  lift $ Substrate.echo "Vault opened, starting shell session..."
  ( do
      lift $ Substrate.changeDir (repositoryDir vri)
      callShell
    )
    `catchError` (\e -> closeVault vri >> throwError e)
  lift $ Substrate.echo "Shell session closed."
  closeVault vri
  lift $ Substrate.echo "Vault closed."

callShell ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
callShell = do
  ExceptT $ Substrate.call "/bin/sh" []

doShellPartition ::
  (Substrate.Substrate m) =>
  String ->
  ExceptT String m ()
doShellPartition partition = do
  vri <- openPartition partition
  lift $ Substrate.echo "Partition opened, starting shell session..."
  ( do
      lift $ Substrate.changeDir (repositoryDir vri)
      callShell
    )
    `catchError` (\e -> closeVault vri >> throwError e)
  lift $ Substrate.echo "Shell session closed."
  closePartition vri
  lift $ Substrate.echo "Partition closed."

-- TODO write tests
doDiffLog ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  ExceptT String m ()
doDiffLog vi = do
  let localLog = (localname vi) ++ ".log"
  let remoteLogs = map (++ ".log") (remotes vi)
  mapM_ (runDiffLog localLog) remoteLogs

runDiffLog ::
  (Substrate.Substrate m) =>
  FilePath ->
  FilePath ->
  ExceptT String m ()
runDiffLog localLog remoteLog = do
  let diffArgs = ["-u", localLog, remoteLog]
  result <- lift $ Substrate.exec "diff" diffArgs ""
  echoDiffResult result

-- `diff` returns code 0 when files are identical and 1 when they differ;
-- for error it returns 2.
echoDiffResult ::
  (Substrate.Substrate m) =>
  Substrate.ExecResult ->
  ExceptT String m ()
echoDiffResult result = do
  case Substrate.exitCode result of
    ExitSuccess -> lift $ Substrate.echo "Log files are identical."
    ExitFailure 1 -> lift $ Substrate.echo (Substrate.output result)
    ExitFailure 2 -> do
      let e = Substrate.errorOutput result
      lift $ Substrate.echo e
      throwError e

-- TODO write extra tests? (see test/TestOperations_Up.hs)
doUploadVault ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  ExceptT String m ()
doUploadVault vi = uploadVaultPartition vi (localname vi)

-- TODO write extra tests? (see test/TestOperations_Up.hs)
uploadVaultPartition ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  FilePath ->
  ExceptT String m ()
uploadVaultPartition vi partition = do
  echoUploadPartition vi partition
  mapM_ (upload vi) [partition ++ ".vault", partition ++ ".log"]
  echoDone

-- TODO write extra tests? (see test/TestOperations_Up.hs)
upload ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  FilePath ->
  ExceptT String m ()
upload vi filename = do
  let remoteFilename = mkpath [(remoteStore vi), (name vi), filename]
  ExceptT $ Substrate.call "rsync" ["-ivz", filename, remoteFilename]

echoUploadPartition ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  FilePath ->
  ExceptT String m ()
echoUploadPartition vi partition = do
  let vaultname = name vi
  let strings = ["Uploading vault partition ", vaultname, "-", partition, "..."]
  lift $ Substrate.echo $ concat strings

-- TODO write tests
doDownloadVault ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  ExceptT String m ()
doDownloadVault vi = mapM_ (downloadVaultPartition vi) (remotes vi)

-- TODO write tests
downloadVaultPartition ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  FilePath ->
  ExceptT String m ()
downloadVaultPartition vi partition = do
  echoDownloadPartition vi partition
  mapM_ (download vi) [partition ++ ".vault", partition ++ ".log"]
  echoDone

echoDownloadPartition ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  FilePath ->
  ExceptT String m ()
echoDownloadPartition vi partition = do
  let vaultname = name vi
  let strings = ["Downloading vault partition ", vaultname, "-", partition, "..."]
  lift $ Substrate.echo $ concat strings

echoDone ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
echoDone = lift $ Substrate.echo "Done."

-- TODO write tests
download ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  FilePath ->
  ExceptT String m ()
download vi filename = do
  let remoteFilename = mkpath [(remoteStore vi), (name vi), filename]
  ExceptT $ Substrate.call "rsync" ["-ivz", remoteFilename, filename]

-- TODO consider alternate procedure (safer?)
-- see lib/Vaults/alternate-sync.txt
doSyncVault ::
  (Substrate.Substrate m) =>
  FilePath ->
  VaultInfo ->
  ExceptT String m ()
doSyncVault remote vi = do
  remoteVRI <- openVault $ remote ++ ".vault"
  (syncLocalPartition vi remoteVRI remote)
    `catchError` (\e -> closeVault remoteVRI >> throwError e)
  closeVault remoteVRI

syncLocalPartition ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  VaultRuntimeInfo ->
  FilePath ->
  ExceptT String m ()
syncLocalPartition vi remoteVRI remote = do
  lift $ Substrate.changeDir (srcDir remoteVRI)
  localVRI <- openVault $ (localname vi) ++ ".vault"
  (performSync localVRI remote)
    `catchError` (\e -> closeVault localVRI >> throwError e)
  closeVault localVRI

doSyncEditVault ::
  (Substrate.Substrate m) =>
  FilePath ->
  VaultInfo ->
  ExceptT String m ()
doSyncEditVault remote vi = do
  remoteVRI <- openVault $ remote ++ ".vault"
  (syncEditLocalPartition vi remoteVRI remote)
    `catchError` (\e -> closeVault remoteVRI >> throwError e)

syncEditLocalPartition ::
  (Substrate.Substrate m) =>
  VaultInfo ->
  VaultRuntimeInfo ->
  FilePath ->
  ExceptT String m ()
syncEditLocalPartition vi remoteVRI remote = do
  lift $ Substrate.changeDir (srcDir remoteVRI)
  localVRI <- openVault $ (localname vi) ++ ".vault"
  (performSync localVRI remote)
    `catchError` (\e -> closeVault localVRI >> throwError e)
  closeVault remoteVRI
  editVault localVRI
  closeVault localVRI

-- TODO test merge conflicts
-- should fail gracefully:
-- -- git fetch remains
-- -- git merge fails
-- -- vault closes automatically
-- merge conflicts can be resolved via operations Edit or Shell
performSync ::
  (Substrate.Substrate m) =>
  VaultRuntimeInfo ->
  FilePath ->
  ExceptT String m ()
performSync localVRI remote = do
  lift $ Substrate.changeDir (repositoryDir localVRI)
  ExceptT $ Substrate.call "git" ["fetch", remote]

  localBranch <- Repo.getCurrentBranch
  let remoteBranch = remote ++ "/" ++ localBranch
  ExceptT $ Substrate.call "git" ["merge", remoteBranch]

mkpath ::
  [String] ->
  String
mkpath = concat . (intersperse "/")
