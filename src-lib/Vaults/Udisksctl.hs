module Vaults.Udisksctl where

import System.Exit
import Control.Monad.Except

import qualified Vaults.Substrate as Substrate

-- TODO validate parameter fname
createLoopDevice :: Substrate.Substrate m => FilePath -> ExceptT String m FilePath
createLoopDevice fname = do
    result <- runUdisksctlCommand ["loop-setup", "-f", fname]
    let parsedDevFile = parseOutputLoopSetup (Substrate.output result)
    case parsedDevFile of
         Left _ -> throwError "loop-setup failed"
         Right devFile -> return devFile

-- TODO validate parameter devFile
unlockDevice :: Substrate.Substrate m => FilePath -> ExceptT String m FilePath
unlockDevice devFile = do
    result <- runUdisksctlCommand ["unlock", "-b", devFile]
    let parsedMapperDev = parseOutputUnlock (Substrate.output result)
    case parsedMapperDev of
         Left _ -> throwError "unlock failed"
         Right mapperDev -> return mapperDev

-- TODO mount as readonly
mountDevice :: Substrate.Substrate m => FilePath -> ExceptT String m FilePath
mountDevice mapperDev = do
    result <- runUdisksctlCommand ["mount", "-b", mapperDev]
    let parsedMountpoint = parseOutputMount (Substrate.output result)
    case parsedMountpoint of
         Left _ -> throwError "mount failed"
         Right mountpoint -> return mountpoint

-- TODO validate parameter mapperDev
unmountDevice :: Substrate.Substrate m => FilePath -> ExceptT String m ()
unmountDevice mapperDev = do
    _ <- runUdisksctlCommand ["unmount", "-b", mapperDev]
    return ()

-- TODO validate parameter mapperDev
lockDevice :: Substrate.Substrate m => FilePath -> ExceptT String m ()
lockDevice mapperDev = do
    _ <- runUdisksctlCommand ["lock", "-b", mapperDev]
    return ()

-- TODO validate parameter devFile
deleteLoopDevice :: Substrate.Substrate m => FilePath -> ExceptT String m ()
deleteLoopDevice devFile = do
    _ <- runUdisksctlCommand ["loop-delete", "-b", devFile]
    return ()

runUdisksctlCommand :: Substrate.Substrate m => [String] -> ExceptT String m Substrate.ExecResult
runUdisksctlCommand params =
    do
        result <- lift $ Substrate.exec "udisksctl" params ""
        when (Substrate.exitCode result /= ExitSuccess)
             (throwError (makeErrorMsg params result))
        return result

makeErrorMsg :: [String] -> Substrate.ExecResult -> String
makeErrorMsg params result =
    if Substrate.exitCode result == ExitSuccess
       then "success"
       else op ++ " failed: " ++ stdErr ++ "\ncommand: " ++ (show params)
            where op = head (params)
                  stdErr = Substrate.errorOutput result

parseOutputLoopSetup = parseUdisksctlOutput True 5
parseOutputUnlock = parseUdisksctlOutput True 4
parseOutputMount = parseUdisksctlOutput False 4

parseUdisksctlOutput :: Bool -> Int -> String -> Either String FilePath
parseUdisksctlOutput endDot nElements outputString = do
    let elements = words outputString
    when (length elements /= nElements) invalidOutput

    let lastElement = last elements
    if endDot then
       do when (last lastElement /= '.') invalidOutput
          when (lastElement == ".") invalidOutput
          let lastElemNoDot = init lastElement
          when (elem '.' lastElemNoDot) invalidOutput
          return lastElemNoDot
    else
       do when (last lastElement == '.') invalidOutput
          return lastElement

invalidOutput :: Either String a
invalidOutput = (Left "invalid udisksctl output")
