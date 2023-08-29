module Substrate where

import System.Directory
import System.Environment
import System.Process

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
    -- for interactive input
    execSub      :: String -> [String] -> ExecResult

instance Substrate IO where
    readFileSub  = Prelude.readFile
    dirExistsSub = System.Directory.doesDirectoryExist
    lookupEnvSub = System.Environment.lookupEnv
    setEnvSub    = System.Environment.setEnv
    unsetEnvSub  = System.Environment.unsetEnv
