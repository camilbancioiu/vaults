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
  case operation of
    InitVault vname local -> runExceptT $ Operations.doInitVault vname local
    ShellPartition partition -> runExceptT $ Operations.doShellPartition partition
    _ -> return $ Left ("operation needs .vault: " ++ (show operation))

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
        DiffLog -> Operations.doDiffLog vi

  runExceptT $ doOperation
