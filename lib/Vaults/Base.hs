module Vaults.Base where

import Data.Maybe
import Data.List.Extra
import Control.Monad.Except

import qualified Vaults.Substrate as Substrate

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
    repositoryDir :: FilePath,
    mountpoint :: FilePath,
    partition :: FilePath,
    partitionName :: FilePath,
    partitionLocation :: PartitionLocation
} deriving (Eq, Show, Read)

loadVaultInfo :: Substrate.Substrate m => m VaultInfo
loadVaultInfo = do
    vname <- Substrate.readFile ".vault/name"
    vlocalname <- Substrate.readFile ".vault/local"
    vremotes <- Substrate.readFile ".vault/remotes"
    vremoteStore <- Substrate.readFile ".vault/remoteStore"

    return VaultInfo {
        name = stripTrailingNewline vname,
        localname = stripTrailingNewline vlocalname,
        remotes = lines vremotes,
        remoteStore = stripTrailingNewline vremoteStore
    }

stripTrailingNewline :: String -> String
stripTrailingNewline s = takeWhile (/='\n') s

isVaultDir :: Substrate.Substrate m => m Bool
isVaultDir = Substrate.dirExists ".vault"

ensureIsVaultDir :: Substrate.Substrate m => ExceptT String m ()
ensureIsVaultDir = do
    isV <- lift $ isVaultDir
    unless isV (throwError "non-vault folder")

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
