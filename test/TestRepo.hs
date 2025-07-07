module TestRepo where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import qualified DummyValues as D
import MockSubstrate
import System.Exit
import Test.HUnit
import qualified Vaults.Base as Base
import Vaults.Repo
import qualified Vaults.Substrate as Sub

allTests :: Test
allTests =
  TestList
    [ test_MissingRepoDir,
      test_UninitializedGit,
      test_IncorrectGitRemotes,
      test_getCurrentBranch,
      test_parseGitRemotes,
      test_getRemotes
    ]

failedGitExecResult =
  D.failedExecResult
    { Sub.exitCode = ExitFailure 128
    }

test_MissingRepoDir :: Test
test_MissingRepoDir =
  TestCase $ do
    let mock = emptyMock
    let vi = mockVaultInfo
    let result = runState (runExceptT $ verifyRepo vi) mock
    let mockAfterExec = snd result

    let expectedCommands = [("dirExists", ["repo"])]

    assertEqualLists
      "check repo commands"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "verify missing repo"
      (Left MissingRepoDir)
      (fst result)

test_UninitializedGit :: Test
test_UninitializedGit =
  TestCase $ do
    let mock = addMockExecResults results mockWithVaultAndRepoDir
          where
            results =
              [ failedGitExecResult
              ]
    let vi = mockVaultInfo
    let result = runState (runExceptT $ verifyRepo vi) mock
    let mockAfterExec = snd result

    let expectedCommands =
          [ ("dirExists", ["repo"]),
            ("git", ["status"])
          ]

    assertEqualLists
      "check repo commands"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "verify uninitialized git repo"
      (Left UninitializedGit)
      (fst result)

test_IncorrectGitRemotes :: Test
test_IncorrectGitRemotes =
  TestCase $ do
    let mock = addMockExecResults ers mockWithVaultAndRepoDir
          where
            ers =
              [ D.successfulExecResult,
                dummyGitRemoteExecResult,
                D.successfulExecResultWithOutput "user"
              ]
    let vi =
          mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB", "remoteC"]
            }
    let result = runState (runExceptT $ verifyRepo vi) mock
    let mockAfterExec = snd result
    let expectedCommands =
          [ ("dirExists", ["repo"]),
            ("git", ["status"]),
            ("git", ["remote", "--verbose"]),
            ("id", ["--user", "--name"])
          ]

    assertEqual
      "check repo commands"
      expectedCommands
      (execRecorded mockAfterExec)

    let expectedRemotes =
          [ GitRemote "remoteA" "/usr/media/user/mockVault-remoteA/repo",
            GitRemote "remoteB" "/usr/media/user/mockVault-remoteB/repo",
            GitRemote "remoteC" "/usr/media/user/mockVault-remoteC/repo"
          ]
    assertEqual
      "verify missing git remotes"
      (Left $ IncorrectGitRemotes expectedRemotes)
      (fst result)

test_makeExpectedRemotes :: Test
test_makeExpectedRemotes =
  TestCase $ do
    let user = "user"

    let vi =
          mockVaultInfo
            { Base.remotes = []
            }
    assertEqual
      "verify generated expectations of no remotes"
      []
      (makeExpectedRemotes vi user)

    let vi =
          mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB"]
            }
    assertEqual
      "verify generated expectations of remotes"
      dummyGitRemotes2
      (makeExpectedRemotes vi user)

test_getCurrentBranch :: Test
test_getCurrentBranch =
  TestCase $ do
    let mock = addMockExecResults results mockWithVaultAndRepoDir
          where
            results = [D.gitBranchShowCurrentExec True D.localOp]
    let result = runState (runExceptT getCurrentBranch) mock
    let mockAfterExec = snd result
    let expectedCommands = [("git", ["branch", "--show-current"])]

    assertEqualLists
      "verify call to git branch"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "verify current branch value"
      (Right (D.currentBranch D.localOp))
      (fst result)

test_getRemotes :: Test
test_getRemotes =
  TestCase $ do
    let mock = addMockExecResults ers mockWithVaultAndRepoDir
          where
            ers =
              [ D.successfulExecResultWithOutput dummyGitRemoteVOut,
                D.successfulExecResultWithOutput "user"
              ]
    let result = runState (runExceptT getRemotes) mock
    let mockAfterExec = snd result
    let expectedCommands = [("git", ["remote", "--verbose"])]

    assertEqualLists
      "verify call to git remote"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "parsed git remotes"
      (Right dummyGitRemotes)
      (fst result)

test_parseGitRemotes :: Test
test_parseGitRemotes =
  TestCase $ do
    assertEqual
      "parsed empty git remote"
      []
      (parseGitRemotes "")

    let parsedRemotes = parseGitRemotes dummyGitRemoteVOut
    assertEqualLists
      "parsed git remotes"
      dummyGitRemotes
      parsedRemotes

dummyGitRemoteExecResult =
  D.successfulExecResult
    { Sub.output = dummyGitRemoteVOut
    }

dummyGitRemoteVOut :: String
dummyGitRemoteVOut =
  unlines
    [ "remoteA\tssh://remoteA/some/directory",
      "remoteB\t/usr/media/user/mockVault-remoteB/repo"
    ]

dummyGitRemotes :: [GitRemote]
dummyGitRemotes =
  [ GitRemote "remoteA" "ssh://remoteA/some/directory",
    GitRemote "remoteB" "/usr/media/user/mockVault-remoteB/repo"
  ]

dummyGitRemotes2 :: [GitRemote]
dummyGitRemotes2 =
  [ GitRemote "remoteA" "/usr/media/user/mockVault-remoteA/repo",
    GitRemote "remoteB" "/usr/media/user/mockVault-remoteB/repo"
  ]
