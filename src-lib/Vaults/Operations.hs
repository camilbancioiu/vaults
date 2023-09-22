module Vaults.Operations where

import System.Exit
import System.Directory
import Control.Monad.Except
import Data.Maybe

import Vaults.Base
import Vaults.Substrate
import qualified Vaults.OperationParams as P

type OpResult = Either String ()

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
openVault :: Substrate m => P.OpenVault -> m (Either String ())
openVault params = runExceptT $ do
    canOpenVault params

    let mfname = P.partitionFilename params
    when (isNothing mfname) (throwError "partition filename is required")
    let fname = case mfname of
         Nothing -> ""
         Just fname -> fname

    vi <- lift $ loadVaultInfo
    let partLoc = getPartitionLocation vi fname
    when (partLoc == UnknownPartition) (throwError "unknown vault partition")

    createLoopDevice fname

    return ()

createLoopDevice :: Substrate m => FilePath -> ExceptT String m ()
createLoopDevice fname = do
    loopSetup <- lift $ execSub "udisksctl" ["loop-setup", "-f", fname] ""
    unless
        (exitCode loopSetup /= ExitSuccess)
        (throwError $ "loop-setup failed: " ++ (errorOutput loopSetup))
    return ()

canOpenVault :: Substrate m => P.OpenVault -> ExceptT String m ()
canOpenVault _ =
    checkIsVaultDir >> checkIsAnyVaultActive

checkIsVaultDir :: Substrate m => ExceptT String m ()
checkIsVaultDir = do
    isV <- lift $ isVaultDir
    unless isV (throwError "non-vault folder")

checkIsAnyVaultActive :: Substrate m => ExceptT String m ()
checkIsAnyVaultActive = do
    isVA <- lift $ isAnyVaultActive
    when isVA (throwError "vault already open")
