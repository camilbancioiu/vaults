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
    [ test_UninitializedGit,
      test_IncorrectGitRemotes,
      test_IncorrectSafeDirs,
      test_getCurrentBranch,
      test_parseGitRemotes,
      test_getRemotes,
      test_makeExpectedRemotes,
      test_makeExpectedSafeDirs,
      test_SuccessfulVerification,
      test_callGitInit,
      test_eraseGitRemotes
    ]

-- TODO refactor everything

failedGitExecResult =
  D.failedExecResult
    { Sub.exitCode = ExitFailure 128
    }

test_UninitializedGit :: Test
test_UninitializedGit =
  TestCase $ do
    let mock = addMockExecResults results mockWithVaultAndRepoDir
          where
            results =
              [ failedGitExecResult
              ]
    let vi = mockVaultInfo
    let result = runState (runExceptT $ verify vi) mock
    let mockAfterExec = snd result

    let expectedCommands =
          [ ("git", ["status"])
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
                D.successfulExecResultWithOutput D.dummyGitRemoteVOut,
                D.successfulExecResultWithOutput "user"
              ]
    let vi =
          mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB", "remoteC"]
            }
    let result = runState (runExceptT $ verify vi) mock
    let mockAfterExec = snd result
    let expectedCommands =
          [ ("git", ["status"]),
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
      "verify git remotes"
      (Left $ IncorrectGitRemotes expectedRemotes)
      (fst result)

test_IncorrectSafeDirs :: Test
test_IncorrectSafeDirs =
  TestCase $ do
    let mock = addMockExecResults ers mockWithVaultAndRepoDir
          where
            ers =
              [ D.successfulExecResult,
                D.successfulExecResultWithOutput D.dummyGitRemoteVOut,
                D.successfulExecResultWithOutput "user",
                D.successfulExecResultWithOutput "user",
                D.successfulExecResultWithOutput existingSafeDirs
              ]
            existingSafeDirs = "/usr/media/user/mockVault-remoteA/repo/.git"

    let vi =
          mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB"]
            }

    let result = runState (runExceptT $ verify vi) mock
    let mockAfterExec = snd result

    assertEqual
      "verify git safe dirs"
      (Left $ IncorrectGitSafeDirs D.dummyGitSafeDirs)
      (fst result)

test_SuccessfulVerification :: Test
test_SuccessfulVerification =
  TestLabel "successful repo verification" $
    TestCase $ do
      let mock = addMockExecResults ers mockWithVaultAndRepoDir
            where
              ers = D.successfulRepoVerificationExecResults
      let vi =
            mockVaultInfo
              { Base.remotes = ["remoteA", "remoteB"]
              }

      let result = runState (runExceptT $ verify vi) mock
      let mockAfterExec = snd result

      let expectedCommands = D.verifyRepoCmds
      assertEqual
        "check repo commands"
        expectedCommands
        (execRecorded mockAfterExec)

      assertEqual
        "repo verification successful"
        (Right ())
        (fst result)

test_callGitInit :: Test
test_callGitInit =
  TestLabel "callGitInit successful" $
    TestCase $ do
      let mock = mockWithVaultAndRepoDir
      let result = runState (runExceptT $ callGitInit UninitializedGit) mock
      let mockAfterExec = snd result

      let expectedCommands = [("git", ["init"])]
      assertEqual
        "check git init command"
        expectedCommands
        (execRecorded mockAfterExec)

test_eraseGitRemotes :: Test
test_eraseGitRemotes =
  TestLabel "erase git remotes successful" $
    TestCase $ do
      let mock = mockWithVaultAndRepoDir
      let result = runState (runExceptT eraseGitRemotes) mock
      let mockAfterExec = snd result
      assertFailure "not implemented"

test_makeExpectedSafeDirs :: Test
test_makeExpectedSafeDirs =
  TestCase $ do
    let user = "user"

    assertEqual
      "verify generated expectations of no safe dirs"
      []
      (makeExpectedSafeDirs "mockVault" user [])

    assertEqual
      "verify generated expectations of remotes"
      (map ((++ "/.git") . remoteURL) D.dummyGitRemotes)
      (makeExpectedSafeDirs "mockVault" user ["remoteA", "remoteB"])

test_makeExpectedRemotes :: Test
test_makeExpectedRemotes =
  TestCase $ do
    let user = "user"

    assertEqual
      "verify generated expectations of no remotes"
      []
      (makeExpectedRemotes [] user [])
    assertEqual
      "verify generated expectations of remotes"
      D.dummyGitRemotes
      (makeExpectedRemotes "mockVault" user ["remoteA", "remoteB"])

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
              [ D.gitRemoteExec,
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
      (Right D.dummyGitRemotes)
      (fst result)

test_parseGitRemotes :: Test
test_parseGitRemotes =
  TestCase $ do
    assertEqual
      "parsed empty git remote"
      []
      (parseGitRemotes "")

    let parsedRemotes = parseGitRemotes D.dummyGitRemoteVOut
    assertEqualLists
      "parsed git remotes"
      D.dummyGitRemotes
      parsedRemotes
