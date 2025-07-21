module Vaults.Init where

import Control.Monad.Except
import Control.Monad.Trans
import qualified Vaults.Substrate2 as Substrate

initVault ::
  (Substrate.Substrate m) =>
  String ->
  String ->
  ExceptT String m ()
initVault vaultName localName = do
  Substrate.createDir ".vault"
  Substrate.writeFile ".vault/name" vaultName
  Substrate.writeFile ".vault/local" localName
  Substrate.writeFile ".vault/remotes" ""
  Substrate.writeFile ".vault/remoteStore" ""
