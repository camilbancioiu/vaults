module Main where

import Control.Monad.Except
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

main :: IO ()
main = do
    vi <- loadVaultInfo
    operation <- execParser operationsParser

    let doOperation = case operation of
                        EditVault -> doEditVault
                        UploadVault -> doUploadVault
                        DownloadVault -> doError "not implemented"
                        SyncVault _ -> doError "not implemented"

    result <- runExceptT $ doOperation vi
    case result of
         Left errMsg -> putStrLn errMsg
         Right _    -> return ()

doEditVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doEditVault vi = do
    let fname = (localname vi) ++ ".vault"
    let forced = False
    let params = ParamsOpenVault fname forced
    vri <- openVault params
    lift $ Substrate.call "nvim" ["."]
    closeVault vri

doUploadVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doUploadVault vi = do
    let mkpath = concat . (intersperse "/")

    let partition = (localname vi) ++ ".vault"
    let remotePathPartition = mkpath [(remoteStore vi), (name vi), partition]
    lift $ Substrate.call "rsync" ["-ivz", partition, remotePathPartition]

    let logfile = (localname vi) ++ ".log"
    let remotePathLogfile = mkpath [(remoteStore vi), (name vi), logfile]
    lift $ Substrate.call "rsync" ["-ivz", logfile, remotePathLogfile]

doError :: Substrate.Substrate m => String -> VaultInfo -> ExceptT String m ()
doError msg _ = throwError msg
