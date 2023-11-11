module Vaults.OpCloseVault where

import Control.Monad.Except

import Vaults.Substrate
import Vaults.Base
import Vaults.Udisksctl

closeVault :: Substrate m => m (Either String ())
closeVault = runExceptT $ do
    -- TODO save git log
    vri <- ensureIsVaultActive
    lift $ changeDirSub (srcDir vri)
    unmountDevice (mapperDev vri)
    lockDevice (mapperDev vri)
    deleteLoopDevice (loopDev vri)
    lift $ unsetEnvSub activeVaultEnvName
    return ()

saveCommitLog :: Substrate m => VaultRuntimeInfo -> m (Either String ())
saveCommitLog vri = runExceptT $ do
    let dir = srcDir vri
    let local = partitionName vri

    return ()
