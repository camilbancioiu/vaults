module Vaults.Operations where

import Control.Monad.Except
import Data.List

import Vaults.Base
import qualified Vaults.Substrate as Substrate

import Vaults.Init
import Vaults.Open
import Vaults.Close
import qualified Vaults.CustomCfg as Cfg

doInitVault :: Substrate.Substrate m => String -> String -> ExceptT String m ()
doInitVault vaultName localName = do
    initVault vaultName localName

-- TODO write tests
doEditVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doEditVault vi = do
    vri <- openVault $ (localname vi) ++ ".vault"
    (do
        lift $ Substrate.changeDir (repositoryDir vri)
        callEditor vri
        )
        `catchError` (\e -> closeVault vri >> throwError e)
    closeVault vri

callEditor :: Substrate.Substrate m => VaultRuntimeInfo -> ExceptT String m ()
callEditor vri = do
    let cfg = Cfg.defaultEditCfg
    let envkey = fst $ Cfg.envVar cfg
    let envvalue = snd $ Cfg.envVar cfg
    lift $ Substrate.setEnv envkey envvalue
    lift $ Substrate.call (Cfg.editor cfg) (Cfg.editorCLIParams cfg)

-- TODO write tests
doShellVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doShellVault vi = do
    vri <- openVault $ (localname vi) ++ ".vault"
    (do
        lift $ Substrate.changeDir (repositoryDir vri)
        callShell vri
        )
        `catchError` (\e -> closeVault vri >> throwError e)
    closeVault vri

callShell :: Substrate.Substrate m => VaultRuntimeInfo -> ExceptT String m ()
callShell vri = do
    lift $ Substrate.call "/bin/sh" []

-- TODO write tests
doUploadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doUploadVault vi = uploadVaultPartition vi (localname vi)

-- TODO write tests
doDownloadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doDownloadVault vi = mapM_ (downloadVaultPartition vi) (remotes vi)

-- TODO consider alternate procedure (safer?)
-- 1. remote loop-setup
-- 1. local loop-setup
-- 2. remote unlock
-- 2. local unlock
-- 3. remote mount
-- 3. local mount
-- 4. git fetch
-- 5. local unmount
-- 5. remote unmount
-- 6. local lock
-- 6. remote lock
-- 7. local loop-delete
-- 7. remote loop-delete
doSyncVault :: Substrate.Substrate m => FilePath -> VaultInfo -> ExceptT String m ()
doSyncVault remote vi = do
    remoteVRI <- openVault $ remote ++ ".vault"
    (syncLocalPartition vi remoteVRI remote)
        `catchError` (\e -> closeVault remoteVRI >> throwError e)
    closeVault remoteVRI

syncLocalPartition :: Substrate.Substrate m => VaultInfo -> VaultRuntimeInfo -> FilePath -> ExceptT String m ()
syncLocalPartition vi remoteVRI remote = do
    lift $ Substrate.changeDir (srcDir remoteVRI)
    localVRI <- openVault $ (localname vi) ++ ".vault"
    (performSync localVRI remote)
        `catchError` (\e -> closeVault localVRI >> throwError e)
    closeVault localVRI

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
