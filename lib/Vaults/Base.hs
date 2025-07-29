module Vaults.Base where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.List.Extra
import Data.Maybe
import System.Exit
import qualified Vaults.Substrate2 as Substrate

data PartitionLocation
  = LocalPartition
  | RemotePartition
  | UnknownPartition
  deriving (Eq, Show, Read)

repoDirName = "repo"

-- TODO add username and hostname fields
data VaultInfo = VaultInfo
  { name :: String,
    localname :: String,
    remotes :: [String],
    remoteStore :: String
  }
  deriving (Eq, Show)

-- TODO disambiguate fields partition and partitionName
data VaultRuntimeInfo = VaultRuntimeInfo
  { srcDir :: FilePath,
    loopDev :: FilePath,
    mapperDev :: FilePath,
    mountpoint :: FilePath,
    -- TODO rename to partitionFile
    partition :: FilePath,
    partitionName :: FilePath,
    partitionLocation :: PartitionLocation
  }
  deriving (Eq, Show, Read)

loadVaultInfo ::
  (Substrate.Substrate m) =>
  ExceptT String m VaultInfo
loadVaultInfo = do
  vname <- Substrate.readFile ".vault/name"
  vlocalname <- Substrate.readFile ".vault/local"
  vremotes <- Substrate.readFile ".vault/remotes"
  vremoteStore <- Substrate.readFile ".vault/remoteStore"
  return
    VaultInfo
      { name = stripTrailingNewline vname,
        localname = stripTrailingNewline vlocalname,
        remotes = lines vremotes,
        remoteStore = stripTrailingNewline vremoteStore
      }

isVaultDir ::
  (Substrate.Substrate m) =>
  ExceptT String m Bool
isVaultDir = Substrate.dirExists ".vault"

ensureIsVaultDir ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
ensureIsVaultDir = do
  isV <- isVaultDir
  unless isV (throwError "non-vault folder")

getPartitionLocation ::
  VaultInfo ->
  FilePath ->
  PartitionLocation
getPartitionLocation vi fname =
  case stripSuffix ".vault" fname of
    Nothing -> UnknownPartition
    Just "" -> UnknownPartition
    Just p ->
      if p == (localname vi)
        then LocalPartition
        else
          if elem p (remotes vi)
            then RemotePartition
            else UnknownPartition

getHostname ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getHostname = do
  result <- Substrate.exec "hostname" [] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get hostname")
  let hostname = Substrate.output result
  return hostname

getUsername ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getUsername = do
  -- Equivalent to `id --user --name`
  result <- Substrate.exec "id" ["-u", "-n"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get username")
  let username = Substrate.output result
  return username

getGroupname ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getGroupname = do
  -- Equivalent to `id --group --name`
  result <- Substrate.exec "id" ["-g", "-n"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get groupname")
  let groupname = Substrate.output result
  return groupname

stripTrailingNewline ::
  String ->
  String
stripTrailingNewline s = reverse $ dropWhile (== '\n') (reverse s)
