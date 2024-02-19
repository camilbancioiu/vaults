module Vaults.OpOpenVault where

import System.Directory
import System.FilePath.Posix
import Control.Monad.Except
import Data.Maybe

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as U

data ParamsOpenVault = ParamsOpenVault {
    partitionFilename :: FilePath,
    isForcedOpening :: Bool
} deriving (Eq, Show)

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
-- TODO handle isForcedOpening
-- TODO create separate flow for non-repo vaults
-- TODO return the VRI for closeVault
openVault :: Substrate.Substrate m => ParamsOpenVault -> m (Either String Base.VaultRuntimeInfo)
openVault params = runExceptT $ do
    Base.ensureIsVaultDir
    Base.ensureNoVaultActive

    let fname = partitionFilename params
    when (length fname == 0) (throwError "partition filename is required")

    vi <- lift $ Base.loadVaultInfo
    let partLoc = Base.getPartitionLocation vi fname
    when (partLoc == Base.UnknownPartition) (throwError $ "unknown vault partition " ++ fname)

    srcDir <- lift $ Substrate.getDir
    loopDev <- U.createLoopDevice fname
    mapperDev <- guardedUnlockDevice loopDev
    mountpoint <- guardedMountDevice loopDev mapperDev
    lift $ Substrate.changeDir mountpoint

    repoDir <- resolveRepoDir mountpoint
    lift $ Substrate.changeDir repoDir

    let vri = Base.VaultRuntimeInfo {
              Base.srcDir = srcDir
            , Base.loopDev = loopDev
            , Base.mountpoint = mountpoint
            , Base.repositoryDir = repoDir
            , Base.mapperDev = mapperDev
            , Base.partition = fname
            , Base.partitionName = takeBaseName fname
            , Base.partitionLocation = partLoc
        }
    lift $ Substrate.setEnv Base.activeVaultEnvName (show vri)

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
        (\e -> do U.lockDevice mapperDev
                  U.deleteLoopDevice loopDev
                  throwError e)

-- resolveRepoDir returns the path `mountpoint/repo` if it exists, otherwise
-- it returns the path `mountpoint`.
resolveRepoDir :: Substrate.Substrate m => FilePath -> ExceptT String m FilePath
resolveRepoDir mountpoint = do
    -- TODO if ensureIsVaultDir, then the folder repo/.git must exist
    -- TODO ensure correct behavior for the missing repo folder
    hasRepoDir <- lift $ Substrate.dirExists "repo"
    let repoDir = if hasRepoDir then mountpoint ++ "/repo"
                                else mountpoint
    return repoDir
