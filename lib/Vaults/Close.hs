module Vaults.Close where

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
    let needCommitLog = (Base.partitionLocation vri) == Base.LocalPartition

    commitLog <- catchError
                    (do
                        if needCommitLog
                        then extractCommitLog
                        else return ""
                    )
                    (\e -> closeVaultDevice vri >> throwError e)

    closeVaultDevice vri

    if needCommitLog
        then saveCommitLog vri commitLog
        else return ()

closeVaultDevice :: Substrate.Substrate m => Base.VaultRuntimeInfo -> ExceptT String m ()
closeVaultDevice vri = do
    lift $ Substrate.changeDir (Base.srcDir vri)
    lift $ Substrate.delay 1000000
    U.unmountDevice (Base.mapperDev vri)
    lift $ Substrate.delay 500000
    U.lockDevice (Base.mapperDev vri)
    U.deleteLoopDevice (Base.loopDev vri)

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
