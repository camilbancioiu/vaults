module DummyValues where

import System.Exit
import qualified Vaults.Substrate as Sub

data DummyOp = DummyOp {
      partitionName :: FilePath
    , loopDev       :: FilePath
    , mapperDev     :: FilePath
    , mountpoint    :: FilePath
    , commitLog     :: String
}

localOp = DummyOp {
      partitionName = "local.vault"
    , loopDev       = "/dev/loop42"
    , mapperDev     = "/dev/dm-4"
    , mountpoint    = "/mnt/point"
    , commitLog     = "38a3\nfb22\n8c2a\n02ad\n"
}

localOp2 = DummyOp {
      partitionName = "local.vault"
    , loopDev       = "/dev/loop9"
    , mapperDev     = "/dev/dm-2"
    , mountpoint    = "/run/media/user/localhostname/mockVault"
    , commitLog     = "38a3\nab22\n8f2a\n03ac\n"
}

remoteOp = DummyOp {
      partitionName = "remoteA.vault"
    , loopDev       = "/dev/loop84"
    , mapperDev     = "/dev/dm-8"
    , mountpoint    = "/mnt/point2"
    , commitLog     = "38a3\nfb22\n8c2a\n02ad\n"
}

showFailedCmd :: (FilePath, [String]) -> String
showFailedCmd ("git", "log":_) =
    "git log failed: "
showFailedCmd (_, cmd@(subcmd:params)) =
    subcmd ++ " failed: \ncommand: " ++ (show cmd)

editCmd :: DummyOp -> (FilePath, [String])
editCmd _ = ("nvim", [ "--clean"
                     , "."
                     ])

gitFetchCmd :: String -> DummyOp -> (FilePath, [String])
gitFetchCmd remote _ = ("git", ["fetch", remote])

gitLogCmd :: DummyOp -> (FilePath, [String])
gitLogCmd _ = ("git", ["log", "--format=%H"])

openPartitionCmds = [ loopSetupCmd, unlockCmd, mountCmd ]
closePartitionCmds = [ unmountCmd, lockCmd, loopDeleteCmd ]
closePartitionWithLogCmds = gitLogCmd : closePartitionCmds

openPartitionExecOk = [ loopSetupExec True, unlockExec True, mountExec True ]
closePartitionExecOk = [ unmountExec True, lockExec True, loopDeleteExec True ]

loopSetupCmd :: DummyOp -> (FilePath, [String])
loopSetupCmd op = ("udisksctl", ["loop-setup", "-f", partitionName op])

unlockCmd :: DummyOp -> (FilePath, [String])
unlockCmd op = ("udisksctl", ["unlock", "-b", loopDev op])

mountCmd :: DummyOp -> (FilePath, [String])
mountCmd op = ("udisksctl", ["mount", "-b", mapperDev op])

unmountCmd :: DummyOp -> (FilePath, [String])
unmountCmd op = ("udisksctl", ["unmount", "-b", mapperDev op])

lockCmd :: DummyOp -> (FilePath, [String])
lockCmd op = ("udisksctl", ["lock", "-b", loopDev op])

loopDeleteCmd :: DummyOp -> (FilePath, [String])
loopDeleteCmd op = ("udisksctl", ["loop-delete", "-b", loopDev op])

gitLogExec :: Bool -> DummyOp -> Sub.ExecResult
gitLogExec success op =
    if not success
       then failedExecResult
       else successfulExecResult {
                Sub.output = commitLog op
       }

loopSetupExec :: Bool -> DummyOp -> Sub.ExecResult
loopSetupExec success op =
    if not success
       then failedExecResult
       else successfulExecResult {
                Sub.output = "Mapped file " ++ (partitionName op)
                             ++ " as " ++ (loopDev op) ++ "."
            }

loopDeleteExec :: Bool -> DummyOp -> Sub.ExecResult
loopDeleteExec success op =
    if not success
       then failedExecResult
       else successfulExecResult

unlockExec :: Bool -> DummyOp -> Sub.ExecResult
unlockExec success op =
    if not success
       then failedExecResult
       else successfulExecResult {
                Sub.output = "Unlocked " ++ (loopDev op)
                             ++ " as " ++ (mapperDev op) ++ "."
       }

mountExec :: Bool -> DummyOp -> Sub.ExecResult
mountExec success op =
    if not success
       then failedExecResult
       else successfulExecResult {
                Sub.output = "Mounted " ++ (mapperDev op)
                             ++ " at " ++ (mountpoint op)
       }

unmountExec :: Bool -> DummyOp -> Sub.ExecResult
unmountExec success op =
    if not success
       then failedExecResult
       else successfulExecResult

lockExec :: Bool -> DummyOp -> Sub.ExecResult
lockExec success op =
    if not success
       then failedExecResult
       else successfulExecResult {
                Sub.output = "Locked " ++ (loopDev op) ++ "."
       }

successfulExecResult :: Sub.ExecResult
successfulExecResult = Sub.ExecResult {
    Sub.exitCode    = ExitSuccess
    , Sub.output      = ""
    , Sub.errorOutput = ""
}

failedExecResult :: Sub.ExecResult
failedExecResult = Sub.ExecResult {
      Sub.exitCode    = ExitFailure 16
    , Sub.output      = "didn't work"
    , Sub.errorOutput = ""
}
