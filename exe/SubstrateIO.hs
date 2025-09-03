module SubstrateIO where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import qualified System.Directory
import qualified System.Environment
import qualified System.Exit
import qualified System.Process
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

instance Substrate.Substrate IO where
  readFile = adaptException . Prelude.readFile
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
wrappedWriteFile filename contents =
  adaptException $ Prelude.writeFile filename contents

wrappedSetEnv ::
  String -> String -> ExceptT String IO ()
wrappedSetEnv varname value =
  adaptException $ System.Environment.setEnv varname value

callIOProcess ::
  String ->
  [String] ->
  ExceptT String IO ()
callIOProcess cmd args =
  adaptException (System.Process.callProcess cmd args)

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

adaptException ::
  IO a ->
  ExceptT String IO a
adaptException = (withExceptT show) . ExceptT . (try :: IO a -> IO (Either SomeException a))
