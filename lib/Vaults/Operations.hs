module Vaults.Operations where

import Control.Monad.Except
import Data.List

import Vaults.Base
import qualified Vaults.Substrate as Substrate

import Vaults.Open
import Vaults.Close

-- TODO write tests
doEditVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doEditVault vi = do
    vri <- openVault $ (localname vi) ++ ".vault"
    (do lift $ Substrate.changeDir (repositoryDir vri)
        lift $ Substrate.call "nvim" ["."])
        `catchError` (\e -> closeVault vri >> throwError e)
    closeVault vri

-- TODO write tests
doUploadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doUploadVault vi = uploadVaultPartition vi (localname vi)

-- TODO write tests
doDownloadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doDownloadVault vi = mapM_ (downloadVaultPartition vi) (remotes vi)

-- TODO write tests
doSyncVault :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
doSyncVault vi remote = do
    remoteVRI <- openVault $ remote ++ ".vault"
    (syncLocalPartition vi remoteVRI remote)
        `catchError` (\e -> closeVault remoteVRI >> throwError e)
    closeVault remoteVRI

-- TODO write tests
syncLocalPartition :: Substrate.Substrate m => VaultInfo -> VaultRuntimeInfo -> FilePath -> ExceptT String m ()
syncLocalPartition vi remoteVRI remote = do
    localVRI <- openVault $ (localname vi) ++ ".vault"
    (performSync localVRI remote)
        `catchError` (\e -> closeVault localVRI >> throwError e)
    closeVault localVRI

-- TODO write tests
performSync :: Substrate.Substrate m => VaultRuntimeInfo -> FilePath -> ExceptT String m ()
performSync localVRI remote = do
    lift $ Substrate.changeDir (repositoryDir localVRI)
    lift $ Substrate.call "git" ["fetch", remote]

-- TODO write tests
uploadVaultPartition :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
uploadVaultPartition vi partition =
    mapM_ (upload vi) [partition ++ ".vault", partition ++ ".log"]

-- TODO write tests
downloadVaultPartition :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
downloadVaultPartition vi partition = do
    mapM_ (download vi) [partition ++ ".vault", partition ++ ".log"]

-- TODO write tests
upload :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
upload vi filename = do
    let remoteFilename = mkpath [(remoteStore vi), (name vi), filename]
    lift $ Substrate.call "rsync" ["-ivz", filename, remoteFilename]

-- TODO write tests
download :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
download vi filename = do
    let remoteFilename = mkpath [(remoteStore vi), (name vi), filename]
    lift $ Substrate.call "rsync" ["-ivz", remoteFilename, filename]

mkpath :: [String] -> String
mkpath = concat . (intersperse "/")
