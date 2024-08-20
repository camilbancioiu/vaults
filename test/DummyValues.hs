module DummyValues where

import Control.Monad.State
import Control.Monad.Except
import Data.List
import qualified MockSubstrate
import System.Exit
import qualified Vaults.Base as Base
import qualified Vaults.CustomCfg as Cfg
import qualified Vaults.Substrate as Sub

data DummyOp = DummyOp
  { partitionFile :: FilePath,
    loopDev :: FilePath,
    mapperDev :: FilePath,
    mountpoint :: FilePath,
    commitLog :: String
  }

localOp =
  DummyOp
    { partitionFile = "local.vault",
      loopDev = "/dev/loop42",
      mapperDev = "/dev/dm-4",
      mountpoint = "/mnt/point",
      commitLog = "38a3\nfb22\n8c2a\n02ad\n"
    }

localOp2 =
  DummyOp
    { partitionFile = "local.vault",
      loopDev = "/dev/loop9",
      mapperDev = "/dev/dm-2",
      mountpoint = "/run/media/user/localhostname/mockVault", -- TODO update URL format
      commitLog = "38a3\nab22\n8f2a\n03ac\n"
    }

remoteOp =
  DummyOp
    { partitionFile = "remoteA.vault",
      loopDev = "/dev/loop84",
      mapperDev = "/dev/dm-8",
      mountpoint = "/mnt/point2",
      commitLog = "38a3\nfb22\n8c2a\n02ad\n"
    }

showFailedCmd :: (FilePath, [String]) -> String
showFailedCmd ("git", "log" : _) =
  "git log failed: "
showFailedCmd (_, cmd@(subcmd : params)) =
  subcmd ++ " failed: \ncommand: " ++ (show cmd)

makeVRI :: DummyOp -> FilePath -> Base.VaultRuntimeInfo
makeVRI op repoDir =
  Base.VaultRuntimeInfo
    { Base.srcDir = Base.srcDir MockSubstrate.mockVaultRuntimeInfo,
      Base.loopDev = loopDev op,
      Base.mapperDev = mapperDev op,
      Base.mountpoint = mountpoint op,
      Base.repositoryDir = (mountpoint op) ++ repoDir,
      Base.partition = partitionFile op,
      Base.partitionName = "local",
      Base.partitionLocation = Base.LocalPartition
    }

editCmd :: DummyOp -> (FilePath, [String])
editCmd _ =
  ( "nvim",
    [ "--clean",
      "--cmd",
      "source .config/nvim/init.vim",
      "."
    ]
  )

openPartitionCmds op =
  [ loopSetupCmd op,
    unlockCmd op,
    mountCmd op
  ]

closePartitionCmds op =
  [ unmountCmd op,
    delayCmd,
    lockCmd op,
    loopDeleteCmd op
  ]

openPartitionExecOk = [loopSetupExec True, unlockExec True, mountExec True]

closePartitionExecOk = [unmountExec True, lockExec True, loopDeleteExec True]

loopSetupCmd :: DummyOp -> (FilePath, [String])
loopSetupCmd op = ("udisksctl", ["loop-setup", "-f", partitionFile op])

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

gitFetchCmd :: String -> DummyOp -> (FilePath, [String])
gitFetchCmd remote _ = ("git", ["fetch", remote])

gitLogCmd :: (FilePath, [String])
gitLogCmd = ("git", ["log", "--format=%H"])

delayCmd :: (FilePath, [String])
delayCmd = ("delay", [])

syncCmd :: (FilePath, [String])
syncCmd = ("sync", [])

changeToSrcDir :: (FilePath, [String])
changeToSrcDir = ("changeDir", [Base.srcDir MockSubstrate.mockVaultRuntimeInfo])

changeToRepoDir :: DummyOp -> (FilePath, [String])
changeToRepoDir op = ("changeDir", [(mountpoint op) ++ "/repo"])

setEnvCmd :: String -> (FilePath, [String])
setEnvCmd varname = ("setEnv", [varname])

preOpenPartitionCmds =
  [ ("dirExists", [".vault"]),
    ("readFile", [".vault/name"]),
    ("readFile", [".vault/local"]),
    ("readFile", [".vault/remotes"]),
    ("readFile", [".vault/remoteStore"]),
    ("getDir", [])
  ]

postOpenPartitionCmds :: DummyOp -> [(FilePath, [String])]
postOpenPartitionCmds op =
  [ ("changeDir", [mountpoint op]),
    ("dirExists", ["repo"])
  ]

preClosePartitionCmds :: [(FilePath, [String])]
preClosePartitionCmds =
  [ changeToSrcDir,
    syncCmd,
    delayCmd
  ]

uploadPartitionCmds :: String -> [(FilePath, [String])]
uploadPartitionCmds partition =
  let vi = MockSubstrate.mockVaultInfo
      partitionFile = partition ++ ".vault"
      partitionLogFile = partition ++ ".log"
      remotePartitionFile = mkpath [(Base.remoteStore vi), (Base.name vi), partitionFile]
      remotePartitionLogFile = mkpath [(Base.remoteStore vi), (Base.name vi), partitionLogFile]
   in [ ("rsync", ["-ivz", partitionFile, remotePartitionFile]),
        ("rsync", ["-ivz", partitionLogFile, remotePartitionLogFile])
      ]

loopSetupExec :: Bool -> DummyOp -> Sub.ExecResult
loopSetupExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output =
            "Mapped file "
              ++ (partitionFile op)
              ++ " as "
              ++ (loopDev op)
              ++ "."
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
    else
      successfulExecResult
        { Sub.output =
            "Unlocked "
              ++ (loopDev op)
              ++ " as "
              ++ (mapperDev op)
              ++ "."
        }

mountExec :: Bool -> DummyOp -> Sub.ExecResult
mountExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output =
            "Mounted "
              ++ (mapperDev op)
              ++ " at "
              ++ (mountpoint op)
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
    else
      successfulExecResult
        { Sub.output = "Locked " ++ (loopDev op) ++ "."
        }

gitLogExec :: Bool -> DummyOp -> Sub.ExecResult
gitLogExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output = commitLog op
        }

gitRemoteExec :: Bool -> DummyOp -> Sub.ExecResult
gitRemoteExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output =
            unlines
              [ "remoteA\t/run/media/user/"
              ]
        }

successfulExecResult :: Sub.ExecResult
successfulExecResult =
  Sub.ExecResult
    { Sub.exitCode = ExitSuccess,
      Sub.output = "",
      Sub.errorOutput = ""
    }

failedExecResult :: Sub.ExecResult
failedExecResult =
  Sub.ExecResult
    { Sub.exitCode = ExitFailure 16,
      Sub.output = "didn't work",
      Sub.errorOutput = ""
    }

mkpath :: [String] -> String
mkpath = concat . (intersperse "/")

dummyOperation ::
  Base.VaultInfo ->
  ExceptT String (State MockSubstrate.Mock) ()
dummyOperation vi = do
  _ <- lift $ Sub.call "dummy" [Base.name vi]
  return ()
