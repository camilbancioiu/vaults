module Vaults.MkRepo where

import Control.Monad.Except

import qualified Vaults.Substrate as Substrate

prepareRepo :: Substrate.Substrate m => ExceptT String m ()
prepareRepo = do
    lift $ Substrate.createDir "repo"
    lift $ Substrate.changeDir "repo"
    ExceptT $ Substrate.call "git" ["init"]

