module Vaults.Substrate2 where

import Control.Concurrent
import Control.Exception
import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Directory
import System.Environment
import System.Exit
import System.Process

data ExecResult = ExecResult
  { exitCode :: ExitCode,
    output :: String,
    errorOutput :: String
  }
  deriving (Eq, Show)

class (Monad m) => Substrate m where
  readFile :: FilePath -> ExceptT String m String
  writeFile :: FilePath -> String -> ExceptT String m ()
  fileExists :: FilePath -> ExceptT String m Bool
  getDir :: ExceptT String m FilePath
  listDirs :: ExceptT String m [FilePath]
  dirExists :: FilePath -> ExceptT String m Bool
  createDir :: FilePath -> ExceptT String m ()
  changeDir :: FilePath -> ExceptT String m ()
  lookupEnv :: String -> ExceptT String m (Maybe String)
  setEnv :: String -> String -> ExceptT String m ()
  unsetEnv :: String -> ExceptT String m ()
  exec :: FilePath -> [String] -> String -> ExceptT String m ExecResult
  call :: FilePath -> [String] -> ExceptT String m ()
  delay :: Int -> ExceptT String m ()
  echo :: String -> ExceptT String m ()
  sync :: ExceptT String m ()

-- TODO consider wrapping all methods in ExceptT
-- TODO e.g. readFile = ExceptT . Prelude.readFile
instance Substrate IO where
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
  echo = lift . putStrLn
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
  ExceptT String IO ExecResult
execIOProcess cmd args sin = do
  let pcmd = (System.Process.proc cmd args)
  result <- lift $ System.Process.readCreateProcessWithExitCode pcmd sin
  let (exc, sout, serr) = result
  return
    ExecResult
      { exitCode = exc,
        output = stripTrailingNewline sout,
        errorOutput = serr
      }

callIOSync :: ExceptT String IO ()
callIOSync = callIOProcess "sync" []

listIODirectories ::
  IO [FilePath]
listIODirectories =
  System.Directory.listDirectory "."
    >>= filterM System.Directory.doesDirectoryExist

stripTrailingNewline ::
  String ->
  String
stripTrailingNewline s = reverse $ dropWhile (== '\n') (reverse s)
