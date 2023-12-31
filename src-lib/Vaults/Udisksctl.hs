module Vaults.Udisksctl where

import System.Exit
import Control.Monad.Except

import Vaults.Substrate

-- TODO validate parameter fname
createLoopDevice :: Substrate m => FilePath -> ExceptT String m FilePath
createLoopDevice fname = do
    let params = ["loop-setup", "-f", fname]
    result <- runUdisksctlCommand params

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
    when (exitCode result /= ExitSuccess) (throwError "mount failed")

    let parsedMountpoint = parseOutputMount (output result)
    case parsedMountpoint of
         Left _ -> throwError "mount failed"
         Right mountpoint -> return mountpoint

-- TODO validate parameter mapperDev
unmountDevice :: Substrate m => FilePath -> ExceptT String m ()
unmountDevice mapperDev = do
    result <- lift $ execSub "udisksctl" ["unmount", "-b", mapperDev] ""
    when (exitCode result /= ExitSuccess) (throwError "unmount failed")

-- TODO validate parameter mapperDev
lockDevice :: Substrate m => FilePath -> ExceptT String m ()
lockDevice mapperDev = do
    result <- lift $ execSub "udisksctl" ["lock", "-b", mapperDev] ""
    when (exitCode result /= ExitSuccess) (throwError "lock failed")
    return ()

-- TODO validate parameter devFile
deleteLoopDevice :: Substrate m => FilePath -> ExceptT String m ()
deleteLoopDevice devFile = do
    result <- lift $ execSub "udisksctl" ["loop-delete", "-b", devFile] ""
    when (exitCode result /= ExitSuccess) (throwError "loop-delete failed")
    return ()

runUdisksctlCommand :: Substrate m => [String] -> ExceptT String m ExecResult
runUdisksctlCommand params =
    do
        result <- lift $ execSub "udisksctl" params ""
        when (exitCode result /= ExitSuccess)
             (throwError (makeErrorMsg params result))
        return result

makeErrorMsg :: [String] -> ExecResult -> String
makeErrorMsg params result =
    if exitCode result == ExitSuccess
       then "success"
       else op ++ " failed: " ++ stdErr ++ "\ncommand: " ++ (show params)
            where op = head (params)
                  stdErr = errorOutput result

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
       do when (elem '.' lastElement) invalidOutput
          return lastElement

invalidOutput :: Either String a
invalidOutput = (Left "invalid udisksctl output")
