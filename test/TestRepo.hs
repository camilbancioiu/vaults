module TestRepo where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import System.Exit
import Control.Monad.Trans
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.Base as Base
import Vaults.Repo
import qualified Vaults.Substrate as Sub

allTests :: Test
allTests =
  TestList
    [ test_MissingRepoDir,
      test_UninitializedGit,
      test_MissingGitRemotes,
      test_getCurrentBranch,
      test_parseGitRemote,
      test_getRemotes
    ]

failedGitExecResult = D.failedExecResult {
  Sub.exitCode = ExitFailure 128
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

test_MissingGitRemotes :: Test
test_MissingGitRemotes =
  TestCase $ do
    let mock = addMockExecResults ers mockWithVaultAndRepoDir
          where
            ers = [ D.successfulExecResult,
                    dummyGitRemoteExecResult ]
    let vi = mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB", "remoteC"]
            }
    let result = runState (runExceptT $ verifyRepo vi) mock
    let mockAfterExec = snd result
    let expectedCommands =
          [ ("dirExists", ["repo"]),
            ("git", ["status"]),
            ("git", ["remote", "--verbose"])
          ]

    assertEqual
      "check repo commands"
      expectedCommands
      (execRecorded mockAfterExec)
    -- TODO test with empty output from git remote
    let expectedMissingRemotes = [GitRemote "remoteC" "/usr/media/user/mockVault-remoteC"]

    assertEqual
      "verify missing git remotes"
      (Left $ MissingGitRemotes expectedMissingRemotes)
      (fst result)

test_makeExpectedRemotes :: Test

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
    let mock = addMockExecResult result mockWithVaultAndRepoDir
          where
            result =
              D.successfulExecResult
                { Sub.output = dummyGitRemoteVOut
                }
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

test_parseGitRemote :: Test
test_parseGitRemote =
  TestCase $ do
    let parsedRemotes = parseGitRemotes dummyGitRemoteVOut
    assertEqualLists
      "parsed Git remotes"
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
      "remoteB\t/usr/media/user/mockVault-remoteB"
    ]

dummyGitRemotes :: [GitRemote]
dummyGitRemotes =
  [ GitRemote "remoteA" "ssh://remoteA/some/directory",
    GitRemote "remoteB" "/usr/media/user/mockVault-remoteB"
  ]
