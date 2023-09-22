module Vaults.Operations where

import System.Directory
import Control.Monad.Except

import Vaults.Base
import Vaults.Substrate
import qualified Vaults.OperationParams as P

type OpResult = Either String ()

-- TODO validate loaded VaultInfo
-- e.g. for empty name, empty localname etc
openVault :: Substrate m => P.OpenVault -> m (Either String ())
openVault params = runExceptT $ do
    canOpenVault params
    v <- lift $ loadVaultInfo
    return ()

canOpenVault :: Substrate m => P.OpenVault -> ExceptT String m ()
canOpenVault _ =
    checkIsVaultDir >> checkIsAnyVaultActive

checkIsVaultDir :: Substrate m => ExceptT String m ()
checkIsVaultDir = do
    isV <- lift $ isVaultDir
    unless isV (throwError "non-vault folder")

checkIsAnyVaultActive :: Substrate m => ExceptT String m ()
checkIsAnyVaultActive = do
    isVA <- lift $ isAnyVaultActive
    when isVA (throwError "vault already open")
