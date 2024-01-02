module Vaults.OpCloseVault where

import Control.Monad.Except
import System.Exit

import qualified Vaults.Base as Base
import Vaults.Substrate
import qualified Vaults.Udisksctl as U

-- TODO take the VRI as parameter; don't read from ENV
closeVault :: Substrate m => m (Either String ())
closeVault = runExceptT $ do
    vri <- Base.ensureIsVaultActive

    result <- lift $ execSub "git" ["log", "--format=%H"] ""
    when (exitCode result /= ExitSuccess) (throwError "git log failed")
    let commitLog = output result

    lift $ changeDirSub (Base.srcDir vri)
    U.unmountDevice (Base.mapperDev vri)
    U.lockDevice (Base.mapperDev vri)
    U.deleteLoopDevice (Base.loopDev vri)
    let logFilename = (Base.partitionName vri) ++ ".log"
    lift $ writeFileSub logFilename commitLog
    lift $ unsetEnvSub Base.activeVaultEnvName
    return ()
