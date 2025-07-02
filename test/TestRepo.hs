module TestRepo where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import Vaults.Repo

allTests :: Test
allTests =
  TestList
    [ test_MissingRepoDir,
      test_UninitializedGit,
      test_getCurrentBranch,
      test_parseGitRemote
    ]

test_MissingRepoDir :: Test
test_MissingRepoDir =
  TestCase $ do
    let mock = emptyMock
    let result = runState (runExceptT verifyRepo) mock
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
              [ D.failedExecResult
              ]
    let result = runState (runExceptT verifyRepo) mock
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

test_parseGitRemote :: Test
test_parseGitRemote =
  TestCase $ do
    let parsedRemotes = parseGitRemotes dummyGitRemoteVOut
    assertEqualLists
      "parsed Git remotes"
      dummyGitRemotes
      parsedRemotes

dummyGitRemoteVOut :: String
dummyGitRemoteVOut =
  unlines
    [ "remoteA\tssh://remoteA/some/directory",
      "remoteB\t/usr/media/user/mockVault-remoteB",
      "remoteC\t/usr/media/user/mockVault-remoteC"
    ]

dummyGitRemotes :: [GitRemote]
dummyGitRemotes =
  [ GitRemote "remoteA" "ssh://remoteA/some/directory",
    GitRemote "remoteB" "/usr/media/user/mockVault-remoteB",
    GitRemote "remoteC" "/usr/media/user/mockVault-remoteC"
  ]
