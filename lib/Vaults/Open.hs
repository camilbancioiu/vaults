module Vaults.Open where

import System.Directory
import System.FilePath.Posix
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad
import Data.Maybe

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as U

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
-- TODO handle isForcedOpening?
-- TODO when partLoc == Base.RemotePartition, mount read-only
-- TODO create separate flow for non-repo vaults
openVault :: Substrate.Substrate m => FilePath -> ExceptT String m Base.VaultRuntimeInfo
openVault partition = do
    Base.ensureIsVaultDir

    when (length partition == 0) (throwError "partition filename is required")

    vi <- lift $ Base.loadVaultInfo
    let partLoc = Base.getPartitionLocation vi partition
    when (partLoc == Base.UnknownPartition) (throwError $ "unknown vault partition " ++ partition)

    vri <- openPartition partition
    let vriWithPartLoc = vri { Base.partitionLocation = partLoc }
    return vriWithPartLoc

openPartition :: Substrate.Substrate m => String -> ExceptT String m Base.VaultRuntimeInfo
openPartition partition = do
    when (length partition == 0) (throwError "partition filename is required")

    srcDir <- lift $ Substrate.getDir
    loopDev <- U.createLoopDevice partition
    mapperDev <- guardedUnlockDevice loopDev
    mountpoint <- guardedMountDevice loopDev mapperDev
    lift $ Substrate.changeDir mountpoint

    repoDir <- resolveRepoDir mountpoint

    let vri = Base.VaultRuntimeInfo {
              Base.srcDir = srcDir
            , Base.loopDev = loopDev
            , Base.mountpoint = mountpoint
            , Base.repositoryDir = repoDir
            , Base.mapperDev = mapperDev
            , Base.partition = partition
            , Base.partitionName = takeBaseName partition
            , Base.partitionLocation = Base.UnknownPartition
        }

    return vri


guardedUnlockDevice :: Substrate.Substrate m => FilePath -> ExceptT String m FilePath
guardedUnlockDevice loopDev = do
    catchError
        (U.unlockDevice loopDev)
        (\e -> do U.deleteLoopDevice loopDev
                  throwError e)

guardedMountDevice :: Substrate.Substrate m => FilePath -> FilePath -> ExceptT String m FilePath
guardedMountDevice loopDev mapperDev = do
    catchError
        (U.mountDevice mapperDev)
        (\e -> do U.lockDevice loopDev
                  U.deleteLoopDevice loopDev
                  throwError e)

resolveRepoDir :: Substrate.Substrate m => FilePath -> ExceptT String m FilePath
resolveRepoDir mountpoint = do
    -- TODO if ensureIsVaultDir, then the folder repo/.git must exist
    -- TODO ensure correct behavior for the missing repo folder
    hasRepoDir <- lift $ Substrate.dirExists "repo"
    let repoDir = if hasRepoDir then mountpoint ++ "/repo"
                                else mountpoint
    return repoDir
