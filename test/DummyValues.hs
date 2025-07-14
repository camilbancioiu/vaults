module DummyValues where

import Control.Monad.Except
import Control.Monad.State
import Data.List
import MockSubstrate
import qualified MockSubstrate
import System.Exit
import qualified Vaults.Base as B
import qualified Vaults.CustomCfg as Cfg
import qualified Vaults.Repo as Repo
import qualified Vaults.Substrate as Sub

data DummyOp = DummyOp
  { partitionFile :: FilePath,
    loopDev :: FilePath,
    mapperDev :: FilePath,
    mountpoint :: FilePath,
    commitLog :: String,
    currentBranch :: String
  }

localOp =
  DummyOp
    { partitionFile = "local.vault",
      loopDev = "/dev/loop42",
      mapperDev = "/dev/mapper/mockVault-local",
      mountpoint = "/mnt/point",
      commitLog = "38a3\nfb22\n8c2a\n02ad\n",
      currentBranch = "themain"
    }

localOp2 =
  DummyOp
    { partitionFile = "local.vault",
      loopDev = "/dev/loop9",
      mapperDev = "/dev/dm-2",
      mountpoint = "/run/media/user/localhostname/mockVault", -- TODO update URL format
      commitLog = "38a3\nab22\n8f2a\n03ac\n",
      currentBranch = "themain"
    }

remoteOp =
  DummyOp
    { partitionFile = "remoteA.vault",
      loopDev = "/dev/loop84",
      mapperDev = "/dev/dm-8",
      mountpoint = "/mnt/point2",
      commitLog = "38a3\nfb22\n8c2a\n02ad\n",
      currentBranch = "themain"
    }

showFailedCmd ::
  (FilePath, [String]) ->
  String
showFailedCmd ("git", "log" : _) =
  "git log failed: "
showFailedCmd (_, cmd@(subcmd : params)) =
  subcmd ++ " failed: \ncommand: " ++ (show cmd)

makeVRI ::
  DummyOp ->
  Bool ->
  B.VaultRuntimeInfo
makeVRI op hasRepoDir =
  B.VaultRuntimeInfo
    { B.srcDir = B.srcDir MockSubstrate.mockVaultRuntimeInfo,
      B.loopDev = loopDev op,
      B.mapperDev = mapperDev op,
      B.mountpoint = mountpoint op,
      B.partition = partitionFile op,
      B.partitionName = "local",
      B.partitionLocation = B.LocalPartition
    }

mockRemoteVRI =
  mockVaultRuntimeInfo
    { B.partitionLocation = B.RemotePartition
    }

editCmd ::
  DummyOp ->
  (FilePath, [String])
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

loopSetupCmd ::
  DummyOp ->
  (FilePath, [String])
loopSetupCmd op = ("udisksctl", ["loop-setup", "-f", partitionFile op])

unlockCmd ::
  DummyOp ->
  (FilePath, [String])
unlockCmd op = ("udisksctl", ["unlock", "-b", loopDev op])

mountCmd ::
  DummyOp ->
  (FilePath, [String])
mountCmd op = ("udisksctl", ["mount", "-b", mapperDev op])

unmountCmd ::
  DummyOp ->
  (FilePath, [String])
unmountCmd op = ("udisksctl", ["unmount", "-b", mapperDev op])

lockCmd ::
  DummyOp ->
  (FilePath, [String])
lockCmd op = ("udisksctl", ["lock", "-b", loopDev op])

loopDeleteCmd ::
  DummyOp ->
  (FilePath, [String])
loopDeleteCmd op = ("udisksctl", ["loop-delete", "-b", loopDev op])

gitFetchCmd ::
  String ->
  DummyOp ->
  (FilePath, [String])
gitFetchCmd remote _ = ("git", ["fetch", remote])

gitMergeCmd ::
  String ->
  DummyOp ->
  (FilePath, [String])
gitMergeCmd remote op = ("git", ["merge", remoteBranch])
  where
    remoteBranch = remote ++ "/" ++ (currentBranch op)

gitBranchShowCurrentCmd ::
  DummyOp ->
  (FilePath, [String])
gitBranchShowCurrentCmd _ = ("git", ["branch", "--show-current"])

gitLogCmd :: (FilePath, [String])
gitLogCmd = ("git", ["log", "--format=%H"])

delayCmd :: (FilePath, [String])
delayCmd = ("delay", [])

syncCmd :: (FilePath, [String])
syncCmd = ("sync", [])

changeToSrcDirCmd :: (FilePath, [String])
changeToSrcDirCmd = ("changeDir", [B.srcDir MockSubstrate.mockVaultRuntimeInfo])

changeToMountpointCmd ::
  DummyOp ->
  (FilePath, [String])
changeToMountpointCmd op = ("changeDir", [(mountpoint op)])

dirExistsRepoCmd = ("dirExists", [B.repoDirName])

createRepoDirCmd = ("createDir", [B.repoDirName])

changeToRepoDirCmd ::
  DummyOp ->
  (FilePath, [String])
changeToRepoDirCmd op = ("changeDir", [B.repoDirName])

ensureRepoDirExistsCmds repoAlreadyExists =
  if repoAlreadyExists
    then [dirExistsRepoCmd]
    else [dirExistsRepoCmd, createRepoDirCmd]

setEnvCmd ::
  String ->
  (FilePath, [String])
setEnvCmd varname = ("setEnv", [varname])

readVaultInfoCmds =
  [ ("readFile", [".vault/name"]),
    ("readFile", [".vault/local"]),
    ("readFile", [".vault/remotes"]),
    ("readFile", [".vault/remoteStore"])
  ]

preOpenPartitionCmds =
  [("dirExists", [".vault"])]
    ++ readVaultInfoCmds
    ++ [("getDir", [])]

postOpenPartitionCmds ::
  DummyOp ->
  [(FilePath, [String])]
postOpenPartitionCmds op =
  [ changeToMountpointCmd op,
    ("tmux", ["rename-window", "mockVault"])
  ]

preClosePartitionCmds :: [(FilePath, [String])]
preClosePartitionCmds =
  [ changeToSrcDirCmd,
    syncCmd,
    delayCmd
  ]

uploadPartitionCmds ::
  String ->
  [(FilePath, [String])]
uploadPartitionCmds partition =
  let vi = MockSubstrate.mockVaultInfo
      partitionFile = partition ++ ".vault"
      partitionLogFile = partition ++ ".log"
      remotePartitionFile = mkpath [(B.remoteStore vi), (B.name vi), partitionFile]
      remotePartitionLogFile = mkpath [(B.remoteStore vi), (B.name vi), partitionLogFile]
   in [ ("rsync", ["-ivz", partitionFile, remotePartitionFile]),
        ("rsync", ["-ivz", partitionLogFile, remotePartitionLogFile])
      ]

makeConformantRepoCmds ::
  [(FilePath, [String])]
makeConformantRepoCmds =
  []
    ++ [ ("git", ["status"]),
         ("git", ["remote"])
       ]
    ++ (map gitRemoteRemoveCmd dummyGitRemoteNames)
    ++ [("id", ["--user", "--name"])]
    ++ (map gitRemoteAddCmd dummyGitRemotes)
    ++ [("git", ["config", "unset", "--local", "--all", "safe.directory"])] -- TODO safe.directory
    ++ (map gitConfigAddSafeDirectory dummyGitRemotes)

gitRemoteRemoveCmd name = ("git", ["remote", "remove", name])

gitRemoteAddCmd gitRemote =
  ( "git",
    [ "remote",
      "add",
      (Repo.remoteName gitRemote),
      (Repo.remoteURL gitRemote)
    ]
  )

gitConfigAddSafeDirectory gitRemote =
  ( "git",
    [ "config",
      "set",
      "--local",
      "--append",
      "safe.directory",
      (Repo.remoteURL gitRemote) ++ "/.git"
    ]
  )

verifyRepoCmds ::
  [(FilePath, [String])]
verifyRepoCmds =
  [ ("git", ["status"]),
    ("git", ["remote", "--verbose"]),
    ("id", ["--user", "--name"]),
    ("id", ["--user", "--name"]),
    ("git", ["config", "get", "--local", "--all", "safe.directory"]) -- TODO safe.directory
  ]

loopSetupExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
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

loopDeleteExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
loopDeleteExec success op =
  if not success
    then failedExecResult
    else successfulExecResult

unlockExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
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

mountExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
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

unmountExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
unmountExec success op =
  if not success
    then failedExecResult
    else successfulExecResult

lockExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
lockExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output = "Locked " ++ (loopDev op) ++ "."
        }

gitLogExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
gitLogExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output = commitLog op
        }

gitBranchShowCurrentExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
gitBranchShowCurrentExec success op =
  if not success
    then failedExecResult
    else
      successfulExecResult
        { Sub.output = currentBranch op
        }

gitMergeExec ::
  Bool ->
  DummyOp ->
  Sub.ExecResult
gitMergeExec success _ =
  if not success
    then failedExecResult
    else successfulExecResult

successfulRepoVerificationExecResults :: [Sub.ExecResult]
successfulRepoVerificationExecResults =
  [ successfulExecResult,
    successfulExecResultWithOutput dummyGitRemoteVOut,
    successfulExecResultWithOutput "user",
    successfulExecResultWithOutput "user",
    successfulExecResultWithOutput (unlines dummyGitSafeDirs)
  ]

successfulRepoMakeConformantExecResults :: [Sub.ExecResult]
successfulRepoMakeConformantExecResults =
  [ successfulExecResult,
    successfulExecResultWithOutput (unlines dummyGitRemoteNames),
    successfulExecResultWithOutput "user"
  ]

gitRemoteExec :: Sub.ExecResult
gitRemoteExec = successfulExecResultWithOutput dummyGitRemoteVOut

dummyGitRemoteVOut :: String
dummyGitRemoteVOut =
  unlines
    [ "remoteA\t/run/media/user/mockVault-remoteA/repo",
      "remoteB\t/run/media/user/mockVault-remoteB/repo"
    ]

dummyGitRemoteNames :: [String]
dummyGitRemoteNames = ["remoteA", "remoteB"]

dummyGitRemotes :: [Repo.GitRemote]
dummyGitRemotes =
  [ Repo.GitRemote "remoteA" "/run/media/user/mockVault-remoteA/repo",
    Repo.GitRemote "remoteB" "/run/media/user/mockVault-remoteB/repo"
  ]

dummyGitSafeDirs :: [FilePath]
dummyGitSafeDirs =
  (map ((++ "/.git") . Repo.remoteURL) dummyGitRemotes)

successfulExecResult :: Sub.ExecResult
successfulExecResult =
  Sub.ExecResult
    { Sub.exitCode = ExitSuccess,
      Sub.output = "",
      Sub.errorOutput = ""
    }

successfulExecResultWithOutput :: String -> Sub.ExecResult
successfulExecResultWithOutput output =
  Sub.ExecResult
    { Sub.exitCode = ExitSuccess,
      Sub.output = output,
      Sub.errorOutput = ""
    }

failedExecResult :: Sub.ExecResult
failedExecResult =
  Sub.ExecResult
    { Sub.exitCode = ExitFailure 16,
      Sub.output = "didn't work",
      Sub.errorOutput = ""
    }

mkpath ::
  [String] ->
  String
mkpath = concat . (intersperse "/")

dummyOperation ::
  B.VaultInfo ->
  ExceptT String (State Mock) ()
dummyOperation vi = do
  let mockOp = mock_call "dummy" [B.name vi]
  lift mockOp >> return ()
