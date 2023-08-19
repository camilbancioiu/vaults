module Substrate where

import System.Directory
import System.Environment

data Substrate = Substrate {
    readFileSub :: (FilePath -> IO String),
    dirExistsSub :: (FilePath -> IO Bool),
    lookupEnvSub :: (String -> IO (Maybe String))
}

realSubstrate = Substrate {
    readFileSub = Prelude.readFile,
    dirExistsSub = System.Directory.doesDirectoryExist,
    lookupEnvSub = System.Environment.lookupEnv
}

undefinedSubstrate = Substrate {
    readFileSub = undefined,
    dirExistsSub = undefined,
    lookupEnvSub = undefined
}

