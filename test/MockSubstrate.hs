{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import Control.Monad.State
import Substrate
import qualified Vaults as V

data Mock = Mock {
    hasVaultDir :: Bool,
    envVars :: [(String, String)]
}

addMockEnvVar :: String -> String -> Mock -> Mock
addMockEnvVar key val mock =
    mock {
        envVars = newEnvVars
    }
    where newEnvVars = (key, val):(envVars mock)

instance Substrate (State Mock) where
    readFileSub  = mock_readFileSub
    dirExistsSub = mock_dirExistsSub
    lookupEnvSub = mock_lookupEnvSub
    setEnvSub    = mock_setEnvSub

mock_readFileSub :: FilePath -> State Mock String
mock_readFileSub ".vault/name" = return (V.name mockVault)
mock_readFileSub ".vault/local" = return (V.localname mockVault)
mock_readFileSub ".vault/remotes" = return (unlines $ V.remotes mockVault)
mock_readFileSub ".vault/remoteStore" = return (V.remoteStore mockVault)

mock_dirExistsSub :: FilePath -> State Mock Bool
mock_dirExistsSub ".vault" = do
    mock <- get
    return (hasVaultDir mock)
mock_dirExistsSub _ = return False

mock_lookupEnvSub :: String -> State Mock (Maybe String)
mock_lookupEnvSub key = do
    mock <- get
    return (lookup key $ envVars mock)

mock_setEnvSub :: String -> String -> State Mock ()
mock_setEnvSub k v = modify (addMockEnvVar k v)

mockVault = V.Vault {
    V.name = "mockVault",
    V.localname = "local",
    V.remotes = ["remoteA", "remoteB"],
    V.remoteStore = "ssh://remoteStore"
}

mockVaultRuntimeInfo = V.VaultRuntimeInfo {
    V.srcDir = "/home/user/vaults/mockVault",
    V.loopDev = "/dev/loop9",
    V.mapperDev = "/dev/dm-2",
    V.mountedRepo = "/run/media/user/localhostname/mockVault",
    V.vaultFile = "local.vault",
    V.isLocalPartition = True
}
