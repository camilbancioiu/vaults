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
    let operation = verify mockVaultInfo

    let expectedCommands =
          [ ("git", ["status"])
          ]

    let mockExecResults = [failedGitExecResult]

    let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    assertEqualLists
      "check repo commands"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "verify uninitialized git repo"
      (Left UninitializedGit)
      (fst operationResult)

    assertAllExecsConsumed mockAfterExec

test_IncorrectGitRemotes :: Test
test_IncorrectGitRemotes =
  TestCase $ do
    let vi =
          mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB", "remoteC"]
            }
    let operation = verify vi

    let expectedCommands =
          [ ("git", ["status"]),
            ("git", ["remote", "--verbose"]),
            ("id", ["-u", "-n"])
          ]

    let mockExecResults =
          [ D.successfulExecResult,
            D.successfulExecResultWithOutput D.dummyGitRemoteVOut,
            D.successfulExecResultWithOutput "user"
          ]

    let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    assertEqual
      "check repo commands"
      expectedCommands
      (execRecorded mockAfterExec)

    let expectedRemotes =
          [ GitRemote "remoteA" "/run/media/user/mockVault-remoteA/repo",
            GitRemote "remoteB" "/run/media/user/mockVault-remoteB/repo",
            GitRemote "remoteC" "/run/media/user/mockVault-remoteC/repo"
          ]

    assertEqual
      "verify git remotes"
      (Left $ IncorrectGitRemotes expectedRemotes)
      (fst operationResult)

test_IncorrectSafeDirs :: Test
test_IncorrectSafeDirs =
  TestCase $ do
    let vi =
          mockVaultInfo
            { Base.remotes = ["remoteA", "remoteB"]
            }

    let operation = verify vi

    let existingSafeDirs = "/run/media/user/mockVault-remoteA/repo/.git"
    let mockExecResults =
          [ D.successfulExecResult,
            D.successfulExecResultWithOutput D.dummyGitRemoteVOut,
            D.successfulExecResultWithOutput "user",
            D.successfulExecResultWithOutput "user",
            D.successfulExecResultWithOutput existingSafeDirs
          ]

    let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir

    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    assertEqual
      "verify git safe dirs"
      (Left $ IncorrectGitSafeDirs D.dummyGitSafeDirs)
      (fst operationResult)

test_SuccessfulVerification :: Test
test_SuccessfulVerification =
  TestLabel "successful repo verification" $
    TestCase $ do
      let operation = verify vi
            where
              vi = mockVaultInfo {Base.remotes = ["remoteA", "remoteB"]}

      let expectedCommands = D.verifyRepoCmds

      let mockExecResults = D.successfulRepoVerificationExecResults

      let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "check repo commands"
        expectedCommands
        (execRecorded mockAfterExec)

      assertEqual
        "repo verification successful"
        (Right ())
        (fst operationResult)

test_callGitInit :: Test
test_callGitInit =
  TestLabel "callGitInit successful" $
    TestCase $ do
      let operation = callGitInit

      let expectedCommands = [("git", ["init"])]

      let mock = mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "check git init command"
        expectedCommands
        (execRecorded mockAfterExec)

test_eraseGitRemotes :: Test
test_eraseGitRemotes =
  TestLabel "erase git remotes successful" $
    TestCase $ do
      let operation = removeGitRemotes

      let expectedCommands =
            [ ("git", ["remote"]),
              ("git", ["remote", "remove", "remoteA"]),
              ("git", ["remote", "remove", "remoteB"]),
              ("git", ["remote", "remove", "remoteX"])
            ]

      let existingRemoteNames = unlines ["remoteA", "remoteB", "remoteX"]
      let mockExecResults = [D.successfulExecResultWithOutput existingRemoteNames]

      let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqualLists
        "commands of removeGitRemotes"
        expectedCommands
        (execRecorded mockAfterExec)

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
    let operation = getCurrentBranch

    let expectedCommands = [("git", ["branch", "--show-current"])]

    let mockExecResults = [D.gitBranchShowCurrentExec True D.localOp]

    let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    assertEqualLists
      "verify call to git branch"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "verify current branch value"
      (Right (D.currentBranch D.localOp))
      (fst operationResult)

test_getRemotes :: Test
test_getRemotes =
  TestCase $ do
    let operation = getRemotes

    let mockExecResults =
          [ D.gitRemoteExec,
            D.successfulExecResultWithOutput "user"
          ]

    let expectedCommands = [("git", ["remote", "--verbose"])]

    let mock = addMockExecResults mockExecResults mockWithVaultAndRepoDir
    let operationResult = runState (runExceptT operation) mock
    let mockAfterExec = snd operationResult

    assertEqualLists
      "verify call to git remote"
      expectedCommands
      (execRecorded mockAfterExec)

    assertEqual
      "parsed git remotes"
      (Right D.dummyGitRemotes)
      (fst operationResult)

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
