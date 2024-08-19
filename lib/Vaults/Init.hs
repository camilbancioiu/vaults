module Vaults.Init where

import Control.Monad.Except
import Control.Monad.Trans
import qualified Vaults.Substrate as Substrate

initVault :: (Substrate.Substrate m) => String -> String -> ExceptT String m ()
initVault vaultName localName = do
  lift $ Substrate.createDir ".vault"
  lift $ Substrate.writeFile ".vault/name" vaultName
  lift $ Substrate.writeFile ".vault/local" localName
  lift $ Substrate.writeFile ".vault/remotes" ""
  lift $ Substrate.writeFile ".vault/remoteStore" ""
