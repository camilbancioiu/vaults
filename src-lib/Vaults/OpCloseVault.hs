module Vaults.OpCloseVault where

import Control.Monad.Except
import System.Exit

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as U

-- TODO take the VRI as parameter; don't read from ENV
closeVault :: Substrate.Substrate m => Base.VaultRuntimeInfo -> ExceptT String m ()
closeVault vri = do
    -- TODO refactor
    -- TODO if this is not a real vault with a git repo, do not extract the
    -- commit log
    commitLog <- catchError
                    extractCommitLog
                    (\e -> do closeVaultDevice vri
                              throwError e)

    closeVaultDevice vri

    saveCommitLog vri commitLog


closeVaultDevice :: Substrate.Substrate m => Base.VaultRuntimeInfo -> ExceptT String m ()
closeVaultDevice vri = do
    lift $ Substrate.changeDir (Base.srcDir vri)
    U.unmountDevice (Base.mapperDev vri)
    U.lockDevice (Base.loopDev vri)
    U.deleteLoopDevice (Base.loopDev vri)
    lift $ Substrate.unsetEnv Base.activeVaultEnvName


extractCommitLog :: Substrate.Substrate m => ExceptT String m String
extractCommitLog = do
    result <- lift $ Substrate.exec "git" ["log", "--format=%H"] ""
    when (Substrate.exitCode result /= ExitSuccess)
         (throwError $ "git log failed: " ++ (Substrate.errorOutput result))
    let commitLog = Substrate.output result
    return commitLog

saveCommitLog :: Substrate.Substrate m => Base.VaultRuntimeInfo -> String -> ExceptT String m ()
saveCommitLog vri commitLog = do
    let logFilename = (Base.partitionName vri) ++ ".log"
    lift $ Substrate.writeFile logFilename commitLog
