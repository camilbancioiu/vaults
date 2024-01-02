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
openVault :: Substrate.Substrate m => ParamsOpenVault -> m (Either String ())
openVault params = runExceptT $ do
    Base.ensureIsVaultDir
    Base.ensureNoVaultActive

    let fname = partitionFilename params
    when (length fname == 0) (throwError "partition filename is required")

    vi <- lift $ Base.loadVaultInfo
    let partLoc = Base.getPartitionLocation vi fname
    when (partLoc == Base.UnknownPartition) (throwError $ "unknown vault partition " ++ fname)

    dirBeforeOpening <- lift $ Substrate.getDir

    devFile <- U.createLoopDevice fname

    mapperDev <- catchError
        (U.unlockDevice devFile)
        (\e -> do
                 U.deleteLoopDevice devFile
                 throwError e)

    mountpoint <- catchError
        (U.mountDevice mapperDev)
        (\e -> do
                 U.lockDevice mapperDev
                 U.deleteLoopDevice devFile
                 throwError e)

    lift $ Substrate.changeDir mountpoint

    -- TODO if ensureIsVaultDir, then the folder repo/.git must exist
    hasRepoDir <- lift $ Substrate.dirExists "repo"
    when hasRepoDir (lift $ Substrate.changeDir "repo")

    let repoDir = if hasRepoDir then mountpoint ++ "/repo"
                                else ""

    let vri = Base.VaultRuntimeInfo {
              Base.srcDir = dirBeforeOpening
            , Base.loopDev = devFile
            , Base.mountpoint = mountpoint
            , Base.repositoryDir = repoDir
            , Base.mapperDev = mapperDev
            , Base.partition = fname
            , Base.partitionName = takeBaseName fname
            , Base.partitionLocation = partLoc
        }

    lift $ Substrate.setEnv Base.activeVaultEnvName (show vri)
