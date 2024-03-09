module Main where

import Control.Monad.Except
import System.Process
import Options.Applicative

import Debug.Trace
import CLI
import SubstrateIO

import Vaults.Base
import qualified Vaults.Substrate as Substrate
import Vaults.OpOpenVault
import Vaults.OpCloseVault

-- TODO operations to implement
-- EditVault, no params, edits the vault in the current folder
-- UploadVault, no params, uploads the local parition to remoteStore
-- DownloadVault, no params, downloads all the remote partitions from remoteStore
-- SyncVault, parameter "remote", mounts local parition and named remote and
--  runs `git fetch` in the local

main :: IO ()
main = do
    vi <- loadVaultInfo
    operation <- execParser operationsParser

    result <- runExceptT $ doEditVault vi
    case result of
         Left errMsg -> putStrLn errMsg
         Right ()    -> return ()

doEditVault :: Substrate.Substrate m => VaultInfo -> ExceptT String m ()
doEditVault vi = do
    let fname = (localname vi) ++ ".vault"
    let forced = False
    let params = ParamsOpenVault fname forced
    vri <- openVault params
    lift $ Substrate.call "nvim" ["."]
    closeVault vri
