{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import System.Exit
import Control.Monad.State
import Vaults.Substrate
import qualified Vaults.Base as V

data Mock = Mock {
    hasVaultDir :: Bool,
    envVars :: [(String, String)],
    nExecs :: Int
}

addMockEnvVar :: String -> String -> Mock -> Mock
addMockEnvVar key val mock =
    mock {
        envVars = newEnvVars
    }
    where newEnvVars = (key, val):(envVars mock)

removeMockEnvVar :: String -> Mock -> Mock
removeMockEnvVar key mock =
    mock {
        envVars = newEnvVars
    }
    where newEnvVars = filter ((key /=) . fst) (envVars mock)

incExecs :: Mock -> Mock
incExecs mock =
    mock {
        nExecs = (nExecs mock) + 1
    }

instance Substrate (State Mock) where
    readFileSub  = mock_readFileSub
    dirExistsSub = mock_dirExistsSub
    lookupEnvSub = mock_lookupEnvSub
    setEnvSub    = mock_setEnvSub
    unsetEnvSub  = mock_unsetEnvSub
    execSub      = mock_execSub

mock_readFileSub :: FilePath -> State Mock String
mock_readFileSub ".vault/name" = return (V.name mockVaultInfo)
mock_readFileSub ".vault/local" = return (V.localname mockVaultInfo)
mock_readFileSub ".vault/remotes" = return (unlines $ V.remotes mockVaultInfo)
mock_readFileSub ".vault/remoteStore" = return (V.remoteStore mockVaultInfo)

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
mock_setEnvSub key val = modify (addMockEnvVar key val)

mock_unsetEnvSub :: String -> State Mock ()
mock_unsetEnvSub key = modify (removeMockEnvVar key)

-- TODO 
mock_execSub :: String -> [String] -> String -> State Mock ExecResult
mock_execSub _ _ _ = do
    modify incExecs
    return $ execResult (ExitSuccess, "", "")

mockVaultInfo = V.VaultInfo {
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
    V.partition = "local.vault",
    V.partitionLocation = V.LocalPartition
}
