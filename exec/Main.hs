module Main where

import Control.Monad.Except
import Control.Exception
import System.Process
import Options.Applicative

import Debug.Trace
import CLI
import SubstrateIO

import Vaults.Base
import qualified Vaults.Substrate as Substrate
import Vaults.Open
import Vaults.Close
import qualified Vaults.Operations as Operations

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
                        EditVault        -> Operations.doEditVault
                        UploadVault      -> Operations.doUploadVault
                        DownloadVault    -> Operations.doDownloadVault
                        SyncVault remote -> Operations.doSyncVault remote
                        DiffLog _        -> doError "not implemented"

    result <- runExceptT $ doOperation vi
    case result of
         Left errMsg -> putStrLn errMsg
         Right _    -> return ()

doError :: Substrate.Substrate m => String -> VaultInfo -> ExceptT String m ()
doError msg _ = throwError msg
