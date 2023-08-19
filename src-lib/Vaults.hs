module Vaults where

import Data.Maybe

import Substrate

activeVaultEnvName :: String
activeVaultEnvName = "ACTIVE_VAULT"

data Vault = Vault {
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
    isLocalPartition :: Bool
} deriving (Eq, Show, Read)

loadVault :: Substrate -> IO Vault
loadVault s = do
    vname <- (readFileSub s) ".vault/name"
    vlocalname <- (readFileSub s) ".vault/local"
    vremotes <- (readFileSub s) ".vault/remotes"
    vremoteStore <- (readFileSub s) ".vault/remoteStore"

    return Vault {
        name = vname,
        localname = vlocalname,
        remotes = lines vremotes,
        remoteStore = vremoteStore
    }

isVaultDir :: Substrate -> IO Bool
isVaultDir s = (dirExistsSub s) ".vault"

getActiveVault :: Substrate -> IO (Maybe VaultRuntimeInfo)
getActiveVault s = do
    descriptor <- (lookupEnvSub s) activeVaultEnvName
    if isNothing descriptor
       then return Nothing
       else return (descriptor >>= read)

isAnyVaultActive :: Substrate -> IO Bool
isAnyVaultActive s = do
    maybeEnv <- (lookupEnvSub s) activeVaultEnvName
    return (isJust maybeEnv)
