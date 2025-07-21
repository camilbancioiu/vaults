module Vaults.Close where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Base as Base
import qualified Vaults.Substrate2 as Substrate
import qualified Vaults.Udisksctl as U

closeVault ::
  (Substrate.Substrate m) =>
  Base.VaultRuntimeInfo ->
  ExceptT String m ()
closeVault vri = do
  -- TODO refactor
  -- TODO if this is not a real vault with a git repo, do not extract the
  -- commit log
  let needCommitLog = (Base.partitionLocation vri) == Base.LocalPartition
  commitLog <-
    catchError
      ( do
          if needCommitLog
            then extractCommitLog
            else return ""
      )
      (\e -> closePartition vri >> throwError e)

  closePartition vri
  if needCommitLog
    then saveCommitLog vri commitLog
    else return ()

closePartition ::
  (Substrate.Substrate m) =>
  Base.VaultRuntimeInfo ->
  ExceptT String m ()
closePartition vri = do
  Substrate.changeDir (Base.srcDir vri)
  Substrate.sync
  Substrate.delay 1000000
  U.unmountDevice (Base.mapperDev vri)
  Substrate.delay 500000
  U.lockDevice (Base.loopDev vri)
  U.deleteLoopDevice (Base.loopDev vri)

extractCommitLog ::
  (Substrate.Substrate m) =>
  ExceptT String m String
extractCommitLog = do
  result <- Substrate.exec "git" ["log", "--format=%H"] ""
  when
    (Substrate.exitCode result /= ExitSuccess)
    (throwError $ "git log failed: " ++ (Substrate.errorOutput result))
  let commitLog = Substrate.output result
  return commitLog

saveCommitLog ::
  (Substrate.Substrate m) =>
  Base.VaultRuntimeInfo ->
  String ->
  ExceptT String m ()
saveCommitLog vri commitLog = do
  let logFilename = (Base.partitionName vri) ++ ".log"
  Substrate.writeFile logFilename commitLog
