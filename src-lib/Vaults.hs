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

loadVault :: Substrate m => m Vault
loadVault = do
    vname <- readFileSub ".vault/name"
    vlocalname <- readFileSub ".vault/local"
    vremotes <- readFileSub ".vault/remotes"
    vremoteStore <- readFileSub ".vault/remoteStore"

    return Vault {
        name = vname,
        localname = vlocalname,
        remotes = lines vremotes,
        remoteStore = vremoteStore
    }

isVaultDir :: Substrate m => m Bool
isVaultDir = dirExistsSub ".vault"

-- getActiveVault :: Substrate -> IO (Maybe VaultRuntimeInfo)
-- getActiveVault s = do
--     descriptor <- (lookupEnvSub s) activeVaultEnvName
--     if isNothing descriptor
--        then return Nothing
--        else return (descriptor >>= read)

-- isAnyVaultActive :: Substrate -> IO Bool
-- isAnyVaultActive s = do
--     maybeEnv <- (lookupEnvSub s) activeVaultEnvName
--     return (isJust maybeEnv)
