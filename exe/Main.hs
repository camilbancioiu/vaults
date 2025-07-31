module Main where

import CLI
import Control.Monad.Except
import Control.Monad.Trans
import Debug.Trace
import Options.Applicative
import SubstrateIO
import System.Process
import Vaults.Base
import Vaults.Close
import qualified Vaults.MultiOperations as MultiOperations
import Vaults.Open
import qualified Vaults.Operations as Operations
import qualified Vaults.Substrate as Substrate

main :: IO ()
main = do
  operation <- execParser operationsParser
  result <- runExceptT $ handleOperation operation
  case result of
    Left errMsg -> putStrLn errMsg
    Right _ -> return ()

handleOperation ::
  Operations.Operation ->
  ExceptT String IO ()
handleOperation operation = do
  isVault <- isVaultDir
  result <-
    if not isVault
      then handleNonVaultOperation operation
      else handleVaultOperation operation
  return result

handleNonVaultOperation ::
  Operations.Operation ->
  ExceptT String IO ()
handleNonVaultOperation operation = do
  lift $ putStrLn $ "Performing non-vault operation " ++ (show operation)
  let doOperation = case operation of
        Operations.InitVault vname local -> Operations.doInitVault vname local
        Operations.ShellPartition partition -> Operations.doShellPartition partition
        Operations.UploadMultiVault -> MultiOperations.doUploadMultiVault
        Operations.DownloadMultiVault -> MultiOperations.doDownloadMultiVault
        Operations.DiffLogMultiVault -> MultiOperations.doDiffLogMultiVault
        _ -> doError "Operation needs .vault: " operation

  doOperation

handleVaultOperation ::
  Operations.Operation ->
  ExceptT String IO ()
handleVaultOperation operation = do
  vi <- loadVaultInfo
  lift $
    putStrLn $
      "Performing operation "
        ++ (show operation)
        ++ " on vault "
        ++ (name vi)
  let doOperation = case operation of
        Operations.MakePartition part sz -> Operations.doMakePartition part sz vi
        Operations.SetupVault -> Operations.doSetupVault vi
        Operations.EditVault -> Operations.doEditVault vi
        Operations.ShellVault -> Operations.doShellVault vi
        Operations.ShellPartition partition -> Operations.doShellPartition partition
        Operations.UploadVault -> Operations.doUploadVault vi
        Operations.DownloadVault -> Operations.doDownloadVault vi
        Operations.SyncVault remote -> Operations.doSyncVault remote vi
        Operations.SyncEditVault remote -> Operations.doSyncEditVault remote vi
        Operations.DiffLog -> Operations.doDiffLog vi
        _ -> doError "Operation unsupported: " operation

  doOperation

doError ::
  (Substrate.Substrate m) =>
  String ->
  Operations.Operation ->
  ExceptT String m ()
doError msg operation = throwError $ concat [msg, show operation]
