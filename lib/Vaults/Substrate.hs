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
    readFile      :: FilePath -> m String
    writeFile     :: FilePath -> String -> m ()
    dirExists     :: FilePath -> m Bool
    fileExists    :: FilePath -> m Bool
    getDir        :: m FilePath
    changeDir     :: FilePath -> m ()
    exec          :: FilePath -> [String] -> String -> m ExecResult
    call          :: FilePath -> [String] -> m ()
    delay         :: Int -> m ()
    echo          :: String -> m ()
