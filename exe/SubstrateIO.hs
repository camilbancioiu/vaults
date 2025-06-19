module SubstrateIO where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.Process
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

-- TODO consider wrapping all methods in ExceptT
-- TODO e.g. readFile = ExceptT . Prelude.readFile
instance Substrate.Substrate IO where
  readFile = Prelude.readFile
  writeFile = Prelude.writeFile
  dirExists = System.Directory.doesDirectoryExist
  fileExists = System.Directory.doesFileExist
  getDir = System.Directory.getCurrentDirectory
  changeDir = System.Directory.setCurrentDirectory
  createDir = System.Directory.createDirectory
  listDirs = listIODirectories
  lookupEnv = System.Environment.lookupEnv
  setEnv = System.Environment.setEnv
  unsetEnv = System.Environment.unsetEnv
  exec = execIOProcess
  call = callIOProcess
  delay = Control.Concurrent.threadDelay
  echo = putStrLn
  sync = callIOSync

callIOProcess ::
  String ->
  [String] ->
  IO (Either String ())
callIOProcess cmd args = do
  catch
    ( do
        System.Process.callProcess cmd args
        return $ Right ()
    )
    (\e -> return $ Left (show $ (e :: SomeException)))

execIOProcess ::
  String ->
  [String] ->
  String ->
  IO Substrate.ExecResult
execIOProcess cmd args sin = do
  let pcmd = (System.Process.proc cmd args)
  result <- System.Process.readCreateProcessWithExitCode pcmd sin
  let (exc, sout, serr) = result
  return
    Substrate.ExecResult
      { Substrate.exitCode = exc,
        Substrate.output = Base.stripTrailingNewline sout,
        Substrate.errorOutput = serr
      }

callIOSync ::
  IO (Either String ())
callIOSync = callIOProcess "sync" []

listIODirectories ::
  IO [FilePath]
listIODirectories =
  System.Directory.listDirectory "."
    >>= filterM System.Directory.doesDirectoryExist
