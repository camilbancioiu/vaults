{-# LANGUAGE FlexibleInstances #-}

module MockSubstrate where

import Control.Monad.State

import Substrate

data MockSubstrate = MockSubstrate {
    hasVaultDir :: Bool
}

instance Substrate (State MockSubstrate) where
    readFileSub = mock_readFileSub
    lookupEnvSub = undefined
    dirExistsSub = mock_dirExistsSub

mock_dirExistsSub :: (MonadState s m) => FilePath -> m Bool
mock_dirExistsSub ".vault" = do
    mock <- get
    return (hasVaultDir mock)
mock_dirExistsSub _ = return False




mock_readFileSub :: (Monad m) => FilePath -> m String
mock_readFileSub ".vault/name" = return "dummy"
mock_readFileSub ".vault/local" = return "local"
mock_readFileSub ".vault/remotes" = return "remoteA\nremoteB"
mock_readFileSub ".vault/remoteStore" = return "remoteStore"
