{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import Control.Monad.State

import Substrate

data Mock = Mock {
    hasVaultDir :: Bool
}

instance Substrate (State Mock) where
    lookupEnvSub = undefined
    dirExistsSub = mock_dirExistsSub
    readFileSub  = mock_readFileSub

mock_dirExistsSub :: FilePath -> State Mock Bool
mock_dirExistsSub ".vault" = do
    mock <- get
    return (hasVaultDir mock)
mock_dirExistsSub _ = return False

mock_readFileSub :: FilePath -> State Mock String
mock_readFileSub ".vault/name" = return "dummy"
mock_readFileSub ".vault/local" = return "local"
mock_readFileSub ".vault/remotes" = return "remoteA\nremoteB"
mock_readFileSub ".vault/remoteStore" = return "ssh://remoteStore"
