module SubstrateIO where

import System.Directory
import System.Environment
import System.Process
import System.Exit
import Control.Concurrent

import qualified Vaults.Substrate as Substrate

instance Substrate.Substrate IO where
    readFile  = Prelude.readFile
    writeFile = Prelude.writeFile
    dirExists = System.Directory.doesDirectoryExist
    getDir    = System.Directory.getCurrentDirectory
    changeDir = System.Directory.setCurrentDirectory
    exec      = execIOProcess
    call      = System.Process.callProcess
    delay     = Control.Concurrent.threadDelay

execIOProcess :: String -> [String] -> String -> IO Substrate.ExecResult
execIOProcess cmd args sin = do
    let pcmd = (proc cmd args)
    result <- readCreateProcessWithExitCode pcmd sin
    let (exc, sout, serr) = result
    return Substrate.ExecResult {
        Substrate.exitCode = exc,
        Substrate.output = sout,
        Substrate.errorOutput = serr
    }
