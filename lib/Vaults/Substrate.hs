module Vaults.Substrate where

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

mkFailMsg ::
  ExecResult ->
  String
mkFailMsg er =
  (show $ exitCode er) ++ ": " ++ (errorOutput er)
