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
      test_UninitializedGit
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
