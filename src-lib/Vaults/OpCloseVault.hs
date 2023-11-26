module Vaults.OpCloseVault where

import Control.Monad.Except
import System.Exit

import Vaults.Substrate
import Vaults.Base
import Vaults.Udisksctl

closeVault :: Substrate m => m (Either String ())
closeVault = runExceptT $ do
    vri <- ensureIsVaultActive

    result <- lift $ execSub "git" ["log", "--format=%H"] ""
    when (exitCode result /= ExitSuccess) (throwError "git log failed")
    let commitLog = output result

    lift $ changeDirSub (srcDir vri)
    unmountDevice (mapperDev vri)
    lockDevice (mapperDev vri)
    deleteLoopDevice (loopDev vri)
    let logfile = (partitionName vri) ++ ".log"
    lift $ writeFileSub logfile commitLog
    lift $ unsetEnvSub activeVaultEnvName
    return ()
