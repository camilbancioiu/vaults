module MockSubstrate where

import Control.Monad.Identity

import Substrate

data MockSubstrate = MockSubstrate {
    hasVaultDir :: Bool
}

mockedVault :: MockSubstrate
mockedVault = MockSubstrate {
    hasVaultDir = True
}

instance Substrate Identity where
    readFileSub = undefined
    dirExistsSub = undefined
    lookupEnvSub = undefined

mockDirExists :: Identity Bool


mockReadFile :: FilePath -> Identity String
mockReadFile ".vault/name" = return "dummy"
mockReadFile ".vault/local" = return "local"
mockReadFile ".vault/remotes" = return "remoteA\nremoteB"
mockReadFile ".vault/remoteStore" = return "remoteStore"
