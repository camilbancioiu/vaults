module Vaults.Base where

import Data.Maybe
import Data.List.Extra
import Vaults.Substrate

activeVaultEnvName :: String
activeVaultEnvName = "ACTIVE_VAULT"

data PartitionLocation = LocalPartition
                       | RemotePartition
                       deriving (Eq, Show, Read)

data VaultInfo = VaultInfo {
    name :: String,
    localname :: String,
    remotes :: [String],
    remoteStore :: String
} deriving (Eq, Show)

data VaultRuntimeInfo = VaultRuntimeInfo {
    srcDir :: FilePath,
    loopDev :: FilePath,
    mapperDev :: FilePath,
    mountedRepo :: FilePath,
    partition :: FilePath,
    partitionLocation :: PartitionLocation
} deriving (Eq, Show, Read)

loadVaultInfo :: Substrate m => m VaultInfo
loadVaultInfo = do
    vname <- readFileSub ".vault/name"
    vlocalname <- readFileSub ".vault/local"
    vremotes <- readFileSub ".vault/remotes"
    vremoteStore <- readFileSub ".vault/remoteStore"

    return VaultInfo {
        name = vname,
        localname = vlocalname,
        remotes = lines vremotes,
        remoteStore = vremoteStore
    }

isVaultDir :: Substrate m => m Bool
isVaultDir = dirExistsSub ".vault"

getActiveVault :: Substrate m => m (Maybe VaultRuntimeInfo)
getActiveVault = do
    descriptor <- lookupEnvSub activeVaultEnvName
    return (fmap read descriptor)

setActiveVault :: Substrate m => VaultRuntimeInfo -> m ()
setActiveVault vri = do
    let descriptor = show vri
    setEnvSub activeVaultEnvName descriptor
    return ()

unsetActiveVault :: Substrate m => m ()
unsetActiveVault = do
    unsetEnvSub activeVaultEnvName
    return ()

isAnyVaultActive :: Substrate m  => m Bool
isAnyVaultActive = do
    maybeEnv <- lookupEnvSub activeVaultEnvName
    return (isJust maybeEnv)
