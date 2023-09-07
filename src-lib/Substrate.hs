module Substrate where

import System.Directory
import System.Environment
import System.Process
import System.Exit

data ExecResult = ExecResult {
    exitCode    :: ExitCode,
    output      :: String,
    errorOutput :: String
    }

execResult (xc, o, eo) = ExecResult {
    exitCode = xc,
    output = o,
    errorOutput = eo
}

class Monad m => Substrate m where
    readFileSub  :: FilePath -> m String
    dirExistsSub :: FilePath -> m Bool
    lookupEnvSub :: String -> m (Maybe String)
    setEnvSub    :: String -> String -> m ()
    unsetEnvSub  :: String -> m ()

    -- TODO implement with readCreateProcessWithExitCode,
    -- which is safe because udisksctl does not use stdout and stdin to request
    -- the passphrase; instead, it accesses the controlling terminal
    -- directly, bypassing the std streams. However, after acquiring the
    -- passphrase, udisksctl will output its result to stdout and stderr like a
    -- normal process.
    execSub      :: String -> [String] -> String -> m ExecResult

instance Substrate IO where
    readFileSub  = Prelude.readFile
    dirExistsSub = System.Directory.doesDirectoryExist
    lookupEnvSub = System.Environment.lookupEnv
    setEnvSub    = System.Environment.setEnv
    unsetEnvSub  = System.Environment.unsetEnv
