module Vaults.Repo where

data RepoVerification
  = MissingRepoDir
  | UninitializedGit
  | MissingGitRemotes [GitRemote]
  | MissingGitSafeDirs [GitRemote]
  | ConformalRepo

data GitRemote = GitRemote
  { remoteName :: String,
    remoteURL :: String
  }
