module Vaults.OpCloseVault where

import Control.Monad.Except
import System.Exit

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as U

-- TODO take the VRI as parameter; don't read from ENV
closeVault :: Substrate.Substrate m => m (Either String ())
closeVault = runExceptT $ do
    vri <- Base.ensureIsVaultActive

    result <- lift $ Substrate.exec "git" ["log", "--format=%H"] ""
    when (Substrate.exitCode result /= ExitSuccess) (throwError "git log failed")
    let commitLog = Substrate.output result

    lift $ Substrate.changeDir (Base.srcDir vri)
    U.unmountDevice (Base.mapperDev vri)
    U.lockDevice (Base.mapperDev vri)
    U.deleteLoopDevice (Base.loopDev vri)
    let logFilename = (Base.partitionName vri) ++ ".log"
    lift $ Substrate.writeFile logFilename commitLog
    lift $ Substrate.unsetEnv Base.activeVaultEnvName
    return ()
