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

    devFile <- createLoopDevice fname

    catchError
        (unlockDevice devFile)
        (\e -> do
                 deleteLoopDevice devFile
                 throwError e)

    return ()

-- TODO validate fname
createLoopDevice :: Substrate m => FilePath -> ExceptT String m FilePath
createLoopDevice fname = do
    result <- lift $ execSub "udisksctl" ["loop-setup", "-f", fname] ""
    when (exitCode result /= ExitSuccess) (throwError "loop-setup failed")

    let parsedDevFile = parseCreateLoopOutput (output result)
    case parsedDevFile of
         Left _ -> throwError "loop-setup failed"
         Right devFile -> return devFile

-- TODO validate devFile
-- TODO parse mapper device from stdout and return it
unlockDevice :: Substrate m => FilePath -> ExceptT String m ()
unlockDevice devFile = do
    result <- lift $ execSub "udisksctl" ["unlock", "-b", devFile] ""
    when (exitCode result /= ExitSuccess) (throwError "unlock failed")
    return ()

-- TODO validate devFile
deleteLoopDevice :: Substrate m => FilePath -> ExceptT String m ()
deleteLoopDevice devFile = do
    result <- lift $ execSub "udisksctl" ["loop-delete", "-b", devFile] ""
    when (exitCode result /= ExitSuccess) (throwError "loop-delete failed")
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

parseCreateLoopOutput :: String -> Either String FilePath
parseCreateLoopOutput output = do
    let elements = words output
    when (length elements /= 5) (Left "invalid loop-setup output")

    let devFileWithDot = last elements
    when (devFileWithDot == ".") (Left "invalid loop-setup output")

    let devFile = init devFileWithDot
    when (elem '.' devFile) (Left "invalid loop-setup output")

    return devFile
