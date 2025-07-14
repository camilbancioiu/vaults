module TestMultiOperations where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.MultiOperations as MultiOperations

allTests :: Test
allTests =
  TestList
    [test_iterateDirs]

test_iterateDirs :: Test
test_iterateDirs =
  TestLabel "iterate into subdirectories and run operation" $
    TestCase $ do
      let operation = MultiOperations.iterateVaultDirs D.dummyOperation
      let mock = mockMultiVault

      let result = runState (runExceptT operation) mock
      let mockAfterExec = snd result

      assertEqual
        "successful iteration"
        (Right ())
        (fst result)

      let expectedCommands =
            [ ("listDirs", []),
              ("dirExists", ["red/.vault"]),
              ("dirExists", ["green/.vault"]),
              ("dirExists", ["blue/.vault"]),
              ("dirExists", ["black/.vault"]),
              ("dirExists", ["white/.vault"])
            ]
              ++ (perVaultCmds "red")
              ++ (perVaultCmds "green")
              ++ (perVaultCmds "blue")

      assertEqualLists
        "commands"
        expectedCommands
        (execRecorded mockAfterExec)

perVaultCmds :: String -> [(FilePath, [String])]
perVaultCmds vault =
  [ ("getDir", []),
    ("changeDir", [vault])
  ]
    ++ D.readVaultInfoCmds
    ++ [ ("dummyOp", [vault]),
         ("changeDir", ["vaults"])
       ]
