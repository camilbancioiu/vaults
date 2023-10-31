module Vaults.OpCloseVault where

import Control.Monad.Except

import Vaults.Substrate
import Vaults.Base

closeVault :: Substrate m => m (Either String ())
closeVault = runExceptT $ do
    vri <- ensureIsVaultActive

    -- TODO save git log

    lift $ changeDirSub (srcDir vri)

    unmountDevice (mapperDev vri)
    lockDevice (loopDev vri)
    deleteLoopDevice (loopDev vri)

    lift $ unsetEnvSub activeVaultEnvName

    return ()
