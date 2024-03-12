module Main where

import Control.Monad.Except
import Control.Exception
import System.Process
import Options.Applicative
import Data.List

import Debug.Trace
import CLI
import SubstrateIO

import Vaults.Base
import qualified Vaults.Substrate as Substrate
import Vaults.OpOpenVault
import Vaults.OpCloseVault

-- TODO operations to implement
-- InitVault, parameter "name" and "local", sets up the .vault folder and files
-- EditVault, no params, edits the vault in the current folder
-- UploadVault, no params, uploads the local parition to remoteStore
-- DownloadVault, no params, downloads all the remote partitions from remoteStore
-- SyncVault, parameter "remote", mounts local parition and named remote and
--  runs `git fetch` in the local
-- DiffLog, parameter "remote", prints the log diff between local.log and remote.log

main :: IO ()
main = do
    vi <- loadVaultInfo
    operation <- execParser operationsParser

    let doOperation = case operation of
                        EditVault -> doEditVault
                        UploadVault -> doUploadVault
                        DownloadVault -> doDownloadVault
                        SyncVault _ -> doError "not implemented"
                        DiffLog _ -> doError "not implemented"

    result <- runExceptT $ doOperation vi
    case result of
         Left errMsg -> putStrLn errMsg
         Right _    -> return ()

doEditVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doEditVault vi = do
    vri <- openVault $ (localname vi) ++ ".vault"
    lift $ Substrate.changeDir (repositoryDir vri)
    lift $ Substrate.call "nvim" ["."]
    closeVault vri

doUploadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doUploadVault vi = do
    uploadVaultPartition vi (localname vi)

doDownloadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doDownloadVault vi = mapM_ (downloadVaultPartition vi) (remotes vi)


doSyncVault :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
doSyncVault vi remote = do
    bracket
        (runExceptT $ openVault $ remote ++ ".vault")
        (runExceptT $ closeVault)
        (\remoteVRI -> bracket
                            (runExceptT $ openVault $ (localname vi) ++ ".vault")
                            (runExceptT $ closeVault)
                            (\localVRI -> runExceptT $ do
                                            lift $ Substrate.changeDir (repositoryDir localVRI)
                                            lift $ Substrate.call "git" ["fetch", remote]))

doError :: Substrate.Substrate m => String -> VaultInfo -> ExceptT String m ()
doError msg _ = throwError msg

uploadVaultPartition :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
uploadVaultPartition vi partition =
    mapM_ (upload vi) [partition ++ ".vault", partition ++ ".log"]

downloadVaultPartition :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
downloadVaultPartition vi partition = do
    mapM_ (download vi) [partition ++ ".vault", partition ++ ".log"]

upload :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
upload vi filename = do
    let remoteFilename = mkpath [(remoteStore vi), (name vi), filename]
    lift $ Substrate.call "rsync" ["-ivz", filename, remoteFilename]

download :: Substrate.Substrate m => VaultInfo -> FilePath -> ExceptT String m ()
download vi filename = do
    let remoteFilename = mkpath [(remoteStore vi), (name vi), filename]
    lift $ Substrate.call "rsync" ["-ivz", remoteFilename, filename]

mkpath :: [String] -> String
mkpath = concat . (intersperse "/")
