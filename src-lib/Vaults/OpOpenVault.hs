module Vaults.OpOpenVault where

import System.Exit
import System.Directory
import Control.Monad.Except
import Data.Maybe

import Vaults.Base
import Vaults.Substrate

data ParamsOpenVault = ParamsOpenVault {
    partitionFilename :: FilePath,
    isForcedOpening :: Bool
} deriving (Eq, Show)

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
openVault :: Substrate m => ParamsOpenVault -> m (Either String ())
openVault params = runExceptT $ do
    canOpenVault params

    let fname = partitionFilename params
    when (length fname == 0) (throwError "partition filename is required")

    vi <- lift $ loadVaultInfo
    let partLoc = getPartitionLocation vi fname
    when (partLoc == UnknownPartition) (throwError "unknown vault partition")

    createLoopDevice fname
    unlockDevice "what"

    return ()


createLoopDevice :: Substrate m => FilePath -> ExceptT String m ()
createLoopDevice fname = do
    result <- lift $ execSub "udisksctl" ["loop-setup", "-f", fname] ""
    when (exitCode result /= ExitSuccess) (throwError "loop-setup failed")
    return ()


unlockDevice :: Substrate m => FilePath -> ExceptT String m ()
unlockDevice dev = do
    result <- lift $ execSub "udisksctl" ["unlock", "-b", dev] ""
    when (exitCode result /= ExitSuccess) (throwError "unlock failed")
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
