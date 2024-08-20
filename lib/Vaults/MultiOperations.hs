module Vaults.MultiOperations where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

doUploadMultiVault :: (Substrate.Substrate m) => ExceptT String m ()
doUploadMultiVault = do
  return ()

iterateVaultSubdirs ::
  (Substrate.Substrate m) =>
  (Base.VaultInfo -> ExceptT String m ()) ->
  ExceptT String m ()
iterateVaultSubdirs doOperation = do
  getVaultSubdirs >>= mapM_ (visitVaultSubdir doOperation)

visitVaultSubdir ::
  (Substrate.Substrate m) =>
  (Base.VaultInfo -> ExceptT String m ()) ->
  FilePath ->
  ExceptT String m ()
visitVaultSubdir doOperation subdir = do
  parentDir <- lift $ Substrate.getDir
  vi <- lift $ prepareOperation subdir
  doOperation vi
  lift $ Substrate.changeDir parentDir
  return ()

prepareOperation :: (Substrate.Substrate m) => FilePath -> m Base.VaultInfo
prepareOperation subdir = do
  Substrate.changeDir subdir
  vi <- Base.loadVaultInfo
  Substrate.echo $
    "Performing operation on vault " ++ (Base.name vi)
  return vi

getVaultSubdirs :: (Substrate.Substrate m) => ExceptT String m [FilePath]
getVaultSubdirs = (lift $ Substrate.listSubdirs) >>= filterM isVaultSubdir

isVaultSubdir :: (Substrate.Substrate m) => FilePath -> ExceptT String m Bool
isVaultSubdir subdir =
  (lift $ Substrate.dirExists vpath) >>= return
  where
    vpath = subdir ++ "/.vault"
