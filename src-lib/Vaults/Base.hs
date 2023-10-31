module Vaults.Base where

import Data.Maybe
import Data.List.Extra
import Control.Monad.Except
import Vaults.Substrate

activeVaultEnvName :: String
activeVaultEnvName = "ACTIVE_VAULT"

data PartitionLocation = LocalPartition
                       | RemotePartition
                       | UnknownPartition
                       deriving (Eq, Show, Read)

type OpResult = Either String ()

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

ensureIsVaultDir :: Substrate m => ExceptT String m ()
ensureIsVaultDir = do
    isV <- lift $ isVaultDir
    unless isV (throwError "non-vault folder")

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

ensureNoVaultActive :: Substrate m => ExceptT String m ()
ensureNoVaultActive = do
    isVA <- lift $ isAnyVaultActive
    when isVA (throwError "vault already open")

-- TODO refactor this
getPartitionLocation :: VaultInfo -> FilePath -> PartitionLocation
getPartitionLocation vi fname =
    case stripSuffix ".vault" fname of
         Nothing -> UnknownPartition
         Just "" -> UnknownPartition
         Just p -> if p == (localname vi)
                      then LocalPartition
                      else if elem p (remotes vi)
                              then RemotePartition
                              else UnknownPartition
