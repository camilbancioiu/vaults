module Vaults.OpOpenVault where

import System.Exit
import System.Directory
import Control.Monad.Except
import Data.Maybe

import Vaults.Base
import Vaults.Substrate

data ParamsOpenVault = ParamsOpenVault {
    partitionFilename :: Maybe FilePath,
    isForcedOpening :: Bool
} deriving (Eq, Show)

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
openVault :: Substrate m => ParamsOpenVault -> m (Either String ())
openVault params = runExceptT $ do
    canOpenVault params

    let mfname = partitionFilename params
    when (isNothing mfname) (throwError "partition filename is required")
    let fname = case mfname of
         Nothing -> ""
         Just fname -> fname

    vi <- lift $ loadVaultInfo
    let partLoc = getPartitionLocation vi fname
    when (partLoc == UnknownPartition) (throwError "unknown vault partition")

    lift $ createLoopDevice fname

    return ()

createLoopDevice :: Substrate m => FilePath -> m (Either String ())
createLoopDevice fname = runExceptT $ do
    loopSetup <- lift $ execSub "udisksctl" ["loop-setup", "-f", fname] ""
    when (exitCode loopSetup /= ExitSuccess) (throwError "loop-setup failed")
    return ()

canOpenVault :: Substrate m => ParamsOpenVault -> ExceptT String m ()
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
