module Main where

import CLI
import Control.Exception
import Control.Monad.Except
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

  isVault <- isVaultDir
  result <-
    if not isVault
      then handleNonVaultOperation operation
      else handleVaultOperation operation

  case result of
    Left errMsg -> putStrLn errMsg
    Right _ -> return ()

handleNonVaultOperation :: Operation -> IO (Either String ())
handleNonVaultOperation operation = do
  putStrLn $ "Performing non-vault operation " ++ (show operation)
  let doOperation = case operation of
        InitVault vname local -> Operations.doInitVault vname local
        ShellPartition partition -> Operations.doShellPartition partition
        UploadMultiVault -> MultiOperations.doUploadMultiVault
        DownloadMultiVault -> MultiOperations.doDownloadMultiVault
        DiffLogMultiVault -> MultiOperations.doDiffLogMultiVault
        _ -> doError "Operation needs .vault: " operation
  runExceptT $ doOperation

handleVaultOperation :: Operation -> IO (Either String ())
handleVaultOperation operation = do
  vi <- loadVaultInfo
  putStrLn $
    "Performing operation "
      ++ (show operation)
      ++ " on vault "
      ++ (name vi)
  let doOperation = case operation of
        MakePartition part sz -> Operations.doMakePartition part sz vi
        EditVault -> Operations.doEditVault vi
        ShellVault -> Operations.doShellVault vi
        ShellPartition partition -> Operations.doShellPartition partition
        UploadVault -> Operations.doUploadVault vi
        DownloadVault -> Operations.doDownloadVault vi
        SyncVault remote -> Operations.doSyncVault remote vi
        SyncEditVault remote -> Operations.doSyncEditVault remote vi
        DiffLog -> Operations.doDiffLog vi
        _ -> doError "Operation unsupported: " operation
  runExceptT $ doOperation

doError :: (Substrate.Substrate m) => String -> Operation -> ExceptT String m ()
doError msg operation = throwError $ concat [msg, show operation]
