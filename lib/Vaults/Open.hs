module Vaults.Open where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe
import System.Directory
import System.FilePath.Posix
import qualified Vaults.Base as B
import qualified Vaults.Repo as Repo
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as U

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
-- TODO handle isForcedOpening?
-- TODO when partLoc == Base.RemotePartition, mount read-only
-- TODO create separate flow for non-repo vaults
openVault ::
  (Substrate.Substrate m) =>
  FilePath ->
  ExceptT String m B.VaultRuntimeInfo
openVault partition = do
  B.ensureIsVaultDir

  when (length partition == 0) (throwError "partition filename is required")

  vi <- B.loadVaultInfo
  let partLoc = B.getPartitionLocation vi partition
  when (partLoc == B.UnknownPartition) (throwError $ "unknown vault partition " ++ partition)

  vri <- openPartition partition
  setTmuxWindowName (B.name vi)

  let vriWithPartLoc = vri {B.partitionLocation = partLoc}

  return vriWithPartLoc

openPartition ::
  (Substrate.Substrate m) =>
  String ->
  ExceptT String m B.VaultRuntimeInfo
openPartition partition = do
  when (length partition == 0) (throwError "partition filename is required")

  srcDir <- Substrate.getDir
  loopDev <- U.createLoopDevice partition
  mapperDev <- guardedUnlockDevice loopDev
  mountpoint <- guardedMountDevice loopDev mapperDev
  Substrate.changeDir mountpoint

  let vri =
        B.VaultRuntimeInfo
          { B.srcDir = srcDir,
            B.loopDev = loopDev,
            B.mountpoint = mountpoint,
            B.mapperDev = mapperDev,
            B.partition = partition,
            B.partitionName = takeBaseName partition,
            B.partitionLocation = B.UnknownPartition
          }

  return vri

guardedUnlockDevice ::
  (Substrate.Substrate m) =>
  FilePath ->
  ExceptT String m FilePath
guardedUnlockDevice loopDev = do
  catchError
    (U.unlockDevice loopDev)
    ( \e -> do
        U.deleteLoopDevice loopDev
        throwError e
    )

guardedMountDevice ::
  (Substrate.Substrate m) =>
  FilePath ->
  FilePath ->
  ExceptT String m FilePath
guardedMountDevice loopDev mapperDev = do
  catchError
    (U.mountDevice mapperDev)
    ( \e -> do
        U.lockDevice loopDev
        U.deleteLoopDevice loopDev
        throwError e
    )

setTmuxWindowName ::
  (Substrate.Substrate m) =>
  String ->
  ExceptT String m ()
setTmuxWindowName name =
  do
    (Substrate.call "tmux" ["rename-window", name])
    `catchError` ( \e -> do
                     Substrate.echo "could not rename tmux window"
                     return ()
                 )
