module Vaults.Operations where

import System.Directory
import Control.Monad.Except

import Vaults.Base
import Vaults.Substrate
import qualified Vaults.OperationParams as P

type OpResult = Either String ()

openVault :: Substrate m => P.OpenVault -> m (Either String ())
openVault params = canOpenVault params

canOpenVault :: Substrate m => P.OpenVault -> m (Either String ())
canOpenVault _ =
    runExceptT (checkIsVaultDir >> checkIsAnyVaultActive)

checkIsVaultDir :: Substrate m => ExceptT String m ()
checkIsVaultDir = do
    isV <- lift $ isVaultDir
    if isV /= True
       then (throwError "non-vault folder")
       else return ()

checkIsAnyVaultActive :: Substrate m => ExceptT String m ()
checkIsAnyVaultActive = do
    isVA <- lift $ isAnyVaultActive
    if isVA == True
       then (throwError "vault already open")
       else return ()
