module Vaults.Substrate where

import System.Directory
import System.Environment
import System.Process
import System.Exit

data ExecResult = ExecResult {
    exitCode    :: ExitCode,
    output      :: String,
    errorOutput :: String
} deriving Show

class Monad m => Substrate m where
    readFileSub      :: FilePath -> m String
    writeFileSub     :: FilePath -> String -> m ()
    dirExistsSub     :: FilePath -> m Bool
    lookupEnvSub     :: String -> m (Maybe String)
    setEnvSub        :: String -> String -> m ()
    unsetEnvSub      :: String -> m ()
    getDirSub        :: m String
    changeDirSub     :: String -> m ()

    -- TODO implement with readCreateProcessWithExitCode,
    -- which is safe because udisksctl does not use stdout and stdin to request
    -- the passphrase; instead, it accesses the controlling terminal
    -- directly, bypassing the std streams. However, after acquiring the
    -- passphrase, udisksctl will output its result to stdout and stderr like a
    -- normal process.
    execSub      :: String -> [String] -> String -> m ExecResult

-- TODO move this in src-exe
instance Substrate IO where
    readFileSub  = Prelude.readFile
    dirExistsSub = System.Directory.doesDirectoryExist
    lookupEnvSub = System.Environment.lookupEnv
    setEnvSub    = System.Environment.setEnv
    unsetEnvSub  = System.Environment.unsetEnv
    execSub      = execIOProcess

execIOProcess :: String -> [String] -> String -> IO ExecResult
execIOProcess cmd args sin = do
    let pcmd = (proc cmd args)
    result <- readCreateProcessWithExitCode pcmd sin
    let (exc, sout, serr) = result
    return ExecResult {
        exitCode = exc,
        output = sout,
        errorOutput = serr
    }
