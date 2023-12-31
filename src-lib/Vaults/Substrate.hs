module Vaults.Substrate where

import System.Directory
import System.Environment
import System.Process
import System.Exit

data ExecResult = ExecResult {
    exitCode    :: ExitCode,
    output      :: String,
    errorOutput :: String
} deriving (Eq, Show)

class Monad m => Substrate m where
    readFileSub      :: FilePath -> m String
    writeFileSub     :: FilePath -> String -> m ()
    dirExistsSub     :: FilePath -> m Bool
    lookupEnvSub     :: String -> m (Maybe String)
    setEnvSub        :: String -> String -> m ()
    unsetEnvSub      :: String -> m ()
    getDirSub        :: m String
    changeDirSub     :: String -> m ()
    execSub          :: String -> [String] -> String -> m ExecResult
