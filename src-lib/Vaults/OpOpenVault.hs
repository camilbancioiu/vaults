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

    mapperDev <- catchError
        (unlockDevice devFile)
        (\e -> do
                 deleteLoopDevice devFile
                 throwError e)

    return ()

-- TODO validate parameter fname
createLoopDevice :: Substrate m => FilePath -> ExceptT String m FilePath
createLoopDevice fname = do
    result <- lift $ execSub "udisksctl" ["loop-setup", "-f", fname] ""
    when (exitCode result /= ExitSuccess) (throwError "loop-setup failed")

    let parsedDevFile = parseOutputLoopSetup (output result)
    case parsedDevFile of
         Left _ -> throwError "loop-setup failed"
         Right devFile -> return devFile

-- TODO validate parameter devFile
unlockDevice :: Substrate m => FilePath -> ExceptT String m FilePath
unlockDevice devFile = do
    result <- lift $ execSub "udisksctl" ["unlock", "-b", devFile] ""
    when (exitCode result /= ExitSuccess) (throwError "unlock failed")

    let parsedMapperDev = parseOutputUnlock (output result)
    case parsedMapperDev of
         Left _ -> throwError "unlock failed"
         Right mapperDev -> return mapperDev

-- TODO mount as readonly
mountDevice :: Substrate m => FilePath -> ExceptT String m FilePath
mountDevice mapperDev = do
    result <- lift $ execSub "udisksctl" ["mount", "-b", mapperDev] ""
    when (exitCode result /= ExitSuccess) (throwError "mounting failed")

    let parsedMountpoint = parseOutputMount (output result)
    case parsedMountpoint of
         Left _ -> throwError "mounting failed"
         Right mountpoint -> return mountpoint

-- TODO validate parameter devFile
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

parseOutputLoopSetup = parseUdisksctlOutput True 5
parseOutputUnlock = parseUdisksctlOutput True 4
parseOutputMount = parseUdisksctlOutput False 4

parseUdisksctlOutput :: Bool -> Int -> String -> Either String FilePath
parseUdisksctlOutput endDot nElements output = do
    let elements = words output
    when (length elements /= nElements) invalidOutput

    let lastElement = last elements
    if endDot then
       do when (not $ elem '.' lastElement) invalidOutput
          when (lastElement == ".") invalidOutput
          let lastElemNoDot = init lastElement
          when (elem '.' lastElemNoDot) invalidOutput
          return lastElemNoDot
    else
        return lastElement

invalidOutput :: Either String a
invalidOutput = (Left "invalid udisksctl output")
