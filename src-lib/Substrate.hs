module Substrate where

import System.Directory
import System.Environment

class Monad m => Substrate m where
    readFileSub  :: FilePath -> m String
    dirExistsSub :: FilePath -> m Bool
    lookupEnvSub :: String -> m (Maybe String)
    setEnvSub    :: String -> String -> m ()
    unsetEnvSub  :: String -> m ()

instance Substrate IO where
    readFileSub  = Prelude.readFile
    dirExistsSub = System.Directory.doesDirectoryExist
    lookupEnvSub = System.Environment.lookupEnv
    setEnvSub    = System.Environment.setEnv
    unsetEnvSub  = System.Environment.unsetEnv
