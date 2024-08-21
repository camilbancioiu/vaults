module Vaults.Substrate where

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

-- TODO consider returning ExceptT from all methods
-- especially from createDir and changeDir
-- TODO function fileExists isn't used anywhere (yet), remove?
class (Monad m) => Substrate m where
  readFile :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
  fileExists :: FilePath -> m Bool
  getDir :: m FilePath
  listDirs :: m [FilePath]
  dirExists :: FilePath -> m Bool
  createDir :: FilePath -> m ()
  changeDir :: FilePath -> m ()
  lookupEnv :: String -> m (Maybe String)
  setEnv :: String -> String -> m ()
  unsetEnv :: String -> m ()
  exec :: FilePath -> [String] -> String -> m ExecResult
  call :: FilePath -> [String] -> m (Either String ())
  delay :: Int -> m ()
  echo :: String -> m ()
  sync :: m (Either String ())
