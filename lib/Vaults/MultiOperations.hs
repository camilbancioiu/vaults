module Vaults.MultiOperations where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import qualified Vaults.Base as Base
import qualified Vaults.Operations as Operations
import qualified Vaults.Substrate2 as Substrate

doUploadMultiVault ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
doUploadMultiVault = iterateVaultDirs Operations.doUploadVault

doDownloadMultiVault ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
doDownloadMultiVault = iterateVaultDirs Operations.doDownloadVault

doDiffLogMultiVault ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
doDiffLogMultiVault = iterateVaultDirs Operations.doDiffLog

iterateVaultDirs ::
  (Substrate.Substrate m) =>
  (Base.VaultInfo -> ExceptT String m ()) ->
  ExceptT String m ()
iterateVaultDirs doOperation = do
  getVaultDirs >>= mapM_ (visitVaultDir doOperation)

visitVaultDir ::
  (Substrate.Substrate m) =>
  (Base.VaultInfo -> ExceptT String m ()) ->
  FilePath ->
  ExceptT String m ()
visitVaultDir doOperation dir = do
  parentDir <- Substrate.getDir
  vi <- prepareOperation dir
  doOperation vi
  Substrate.changeDir parentDir
  return ()

prepareOperation ::
  (Substrate.Substrate m) =>
  FilePath ->
  ExceptT String m Base.VaultInfo
prepareOperation dir = do
  Substrate.changeDir dir
  vi <- Base.loadVaultInfo
  Substrate.echo $
    "\n=== Performing operation on vault " ++ (Base.name vi) ++ " ==="
  return vi

getVaultDirs ::
  (Substrate.Substrate m) =>
  ExceptT String m [FilePath]
getVaultDirs = (Substrate.listDirs) >>= filterM isVaultDir

isVaultDir ::
  (Substrate.Substrate m) =>
  FilePath ->
  ExceptT String m Bool
isVaultDir dir =
  (Substrate.dirExists vpath) >>= return
  where
    vpath = dir ++ "/.vault"
