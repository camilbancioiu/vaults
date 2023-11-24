module Vaults.OpCloseVault where

import Control.Monad.Except
import System.Exit

import Vaults.Substrate
import Vaults.Base
import Vaults.Udisksctl

closeVault :: Substrate m => m (Either String ())
closeVault = runExceptT $ do
    vri <- ensureIsVaultActive

    let dir = srcDir vri
    let localname = partitionName vri
    let logfile = dir ++ "/" ++ localname ++ ".log"
    result <- lift $ execSub "git" ["log", "--format=%H"] ""
    when (exitCode result /= ExitSuccess) (throwError "git log failed")
    let commitLog = output result
    lift $ writeFileSub logfile commitLog

    lift $ changeDirSub (srcDir vri)
    unmountDevice (mapperDev vri)
    lockDevice (mapperDev vri)
    deleteLoopDevice (loopDev vri)
    lift $ unsetEnvSub activeVaultEnvName
    return ()
