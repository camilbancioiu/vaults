module Vaults where

import System.Directory
import System.Environment

ActvieVaultEnv :: String
ActvieVaultEnv = "ACTIVE_VAULT"

data Substrate = Substrate {
    subReadFile :: (FilePath -> IO String),
    subDirExists :: (FilePath -> IO Bool)
}

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

readVault :: Substrate -> IO Vault
readVault s = do
    vname <- (subReadFile s) ".vault/name"
    vlocalname <- (subReadFile s) ".vault/local"
    vremotes <- (subReadFile s) ".vault/remotes"
    vremoteStore <- (subReadFile s) ".vault/remoteStore"

    return Vault {
        name = vname,
        localname = vlocalname,
        remotes = lines vremotes,
        remoteStore = vremoteStore
    }

isVaultDir :: Substrate -> IO Bool
isVaultDir s = (subDirExists s) ".vault"

getActiveVault 

isAnyVaultActive :: Substrate -> IO Bool
isAnyVaultActive s = 
