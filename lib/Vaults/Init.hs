module Vaults.Init where

import Control.Monad.Except
import qualified Vaults.Substrate as Substrate

initVault :: Substrate.Substrate m => String -> String -> ExceptT String m ()
initVault vaultName localName = do
    return ()
