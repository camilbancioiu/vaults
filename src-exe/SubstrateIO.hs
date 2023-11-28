module SubstrateIO where

import System.Directory
import System.Environment
import System.Process
import System.Exit

import Vaults.Substrate

instance Substrate IO where
    readFileSub  = Prelude.readFile
    writeFileSub = Prelude.writeFile
    dirExistsSub = System.Directory.doesDirectoryExist
    lookupEnvSub = System.Environment.lookupEnv
    setEnvSub    = System.Environment.setEnv
    unsetEnvSub  = System.Environment.unsetEnv
    getDirSub    = System.Directory.getCurrentDirectory
    changeDirSub = System.Directory.setCurrentDirectory
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
