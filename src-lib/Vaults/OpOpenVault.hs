module Vaults.OpOpenVault where

import System.Directory
import Control.Monad.Except
import Data.Maybe

import Vaults.Base
import Vaults.Substrate
import Vaults.Udisksctl

data ParamsOpenVault = ParamsOpenVault {
    partitionFilename :: FilePath,
    isForcedOpening :: Bool
} deriving (Eq, Show)

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
openVault :: Substrate m => ParamsOpenVault -> m (Either String ())
openVault params = runExceptT $ do
    checkIsVaultDir
    checkIsAnyVaultActive

    let fname = partitionFilename params
    when (length fname == 0) (throwError "partition filename is required")

    vi <- lift $ loadVaultInfo
    let partLoc = getPartitionLocation vi fname
    when (partLoc == UnknownPartition) (throwError "unknown vault partition")

    dirBeforeOpening <- lift $ getDirSub
    devFile <- createLoopDevice fname

    mapperDev <- catchError
        (unlockDevice devFile)
        (\e -> do
                 deleteLoopDevice devFile
                 throwError e)

    mountPoint <- catchError
        (mountDevice mapperDev)
        (\e -> do
                 lockDevice mapperDev
                 deleteLoopDevice devFile
                 throwError e)

    lift $ changeDirSub mountPoint

    hasRepoDir <- lift $ dirExistsSub "repo"
    when hasRepoDir (lift $ changeDirSub "repo")

    let repoDir = if hasRepoDir then mountPoint ++ "/repo"
                                else mountPoint

    let vri = VaultRuntimeInfo {
              srcDir = dirBeforeOpening
            , loopDev = devFile
            , mapperDev = mapperDev
            , mountedRepo = repoDir
            , partition = fname
            , partitionLocation = partLoc
        }

    lift $ setEnvSub activeVaultEnvName (show vri)


checkIsVaultDir :: Substrate m => ExceptT String m ()
checkIsVaultDir = do
    isV <- lift $ isVaultDir
    unless isV (throwError "non-vault folder")

checkIsAnyVaultActive :: Substrate m => ExceptT String m ()
checkIsAnyVaultActive = do
    isVA <- lift $ isAnyVaultActive
    when isVA (throwError "vault already open")
