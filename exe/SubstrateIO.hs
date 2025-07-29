module SubstrateIO where

import Control.Concurrent
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.Process
import qualified Vaults.Base as Base
import qualified Vaults.Substrate2 as Substrate

instance Substrate.Substrate IO where
  readFile = lift . Prelude.readFile
  writeFile = wrappedWriteFile
  dirExists = lift . System.Directory.doesDirectoryExist
  fileExists = lift . System.Directory.doesFileExist
  getDir = ExceptT $ (fmap Right) System.Directory.getCurrentDirectory
  changeDir = lift . System.Directory.setCurrentDirectory
  createDir = lift . System.Directory.createDirectory
  listDirs = ExceptT $ (fmap Right) listIODirectories
  lookupEnv = lift . System.Environment.lookupEnv
  setEnv = wrappedSetEnv
  unsetEnv = lift . System.Environment.unsetEnv
  exec = execIOProcess
  call = callIOProcess
  delay = lift . Control.Concurrent.threadDelay
  echo = lift . Prelude.putStrLn
  sync = callIOSync

wrappedWriteFile ::
  FilePath ->
  String ->
  ExceptT String IO ()
wrappedWriteFile filename contents = lift $ Prelude.writeFile filename contents

wrappedSetEnv ::
  String -> String -> ExceptT String IO ()
wrappedSetEnv varname value = lift $ System.Environment.setEnv varname value

callIOProcess ::
  String ->
  [String] ->
  ExceptT String IO ()
callIOProcess cmd args =
  catchError
    ( do
        lift $ System.Process.callProcess cmd args
        return ()
    )
    (\e -> throwError (show e))

execIOProcess ::
  String ->
  [String] ->
  String ->
  ExceptT String IO Substrate.ExecResult
execIOProcess cmd args sin = do
  let pcmd = (System.Process.proc cmd args)
  result <- lift $ System.Process.readCreateProcessWithExitCode pcmd sin
  let (exc, sout, serr) = result
  return
    Substrate.ExecResult
      { Substrate.exitCode = exc,
        Substrate.output = Base.stripTrailingNewline sout,
        Substrate.errorOutput = serr
      }

callIOSync :: ExceptT String IO ()
callIOSync = callIOProcess "sync" []

listIODirectories ::
  IO [FilePath]
listIODirectories =
  System.Directory.listDirectory "."
    >>= filterM System.Directory.doesDirectoryExist
