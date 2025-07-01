module Vaults.Base where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.List.Extra
import Data.Maybe
import System.Exit
import qualified Vaults.Substrate as Substrate

data PartitionLocation
  = LocalPartition
  | RemotePartition
  | UnknownPartition
  deriving (Eq, Show, Read)

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
    repositoryDir :: FilePath,
    mountpoint :: FilePath,
    -- TODO rename to partitionFile
    partition :: FilePath,
    partitionName :: FilePath,
    partitionLocation :: PartitionLocation
  }
  deriving (Eq, Show, Read)

loadVaultInfo ::
  (Substrate.Substrate m) =>
  m VaultInfo
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
  m Bool
isVaultDir = Substrate.dirExists ".vault"

ensureIsVaultDir ::
  (Substrate.Substrate m) =>
  ExceptT String m ()
ensureIsVaultDir = do
  isV <- lift $ isVaultDir
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
  result <- lift $ Substrate.exec "hostname" [] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get hostname")
  let hostname = Substrate.output result
  return hostname

getUsername ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getUsername = do
  result <- lift $ Substrate.exec "id" ["--user", "--name"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get username")
  let username = Substrate.output result
  return username

getGroupname ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getGroupname = do
  result <- lift $ Substrate.exec "id" ["--group", "--name"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get groupname")
  let groupname = Substrate.output result
  return groupname

getCurrentBranch ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getCurrentBranch = do
  result <- lift $ Substrate.exec "git" ["branch", "--show-current"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get current branch")
  let currentBranch = Substrate.output result
  return currentBranch

stripTrailingNewline ::
  String ->
  String
stripTrailingNewline s = reverse $ dropWhile (== '\n') (reverse s)
