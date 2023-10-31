module Vaults.OpCloseVault where

import Vaults.Base

closeVault :: Substrate m => m (Either String ())
closeVault = runExceptT $ do
    ensureIsVaultActive

    vri <- lift $ getActiveVault
