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

main :: IO ()
main = do
    operation <- execParser operationsParser

    isVault <- isVaultDir
    result <- if not isVault then handleNonVaultOperation operation
                             else handleVaultOperation operation

    case result of
         Left errMsg -> putStrLn errMsg
         Right _    -> return ()

handleNonVaultOperation :: Operation -> IO (Either String ())
handleNonVaultOperation operation =
    case operation of
         InitVault vname local -> runExceptT $ Operations.doInitVault vname local
         ShellPartition partition -> runExceptT $ Operations.doShellPartition partition
         _ -> return $ Left ("operation needs .vault: " ++ (show operation))

handleVaultOperation :: Operation -> IO (Either String ())
handleVaultOperation operation = do
    vi <- loadVaultInfo
    let doOperation = case operation of
                        MakePartition part sz    -> Operations.doMakePartition part sz vi
                        EditVault                -> Operations.doEditVault vi
                        ShellVault               -> Operations.doShellVault vi
                        ShellPartition partition -> Operations.doShellPartition partition
                        UploadVault              -> Operations.doUploadVault vi
                        DownloadVault            -> Operations.doDownloadVault vi
                        SyncVault remote         -> Operations.doSyncVault remote vi
                        DiffLog remote           -> Operations.doDiffLog remote vi

    runExceptT $ doOperation

doError :: Substrate.Substrate m => String -> VaultInfo -> ExceptT String m ()
doError msg _ = throwError msg
