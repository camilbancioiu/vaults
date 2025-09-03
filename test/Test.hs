module Main (main) where

import System.Exit
import Test.HUnit
import qualified TestBase (allTests)
import qualified TestClose (allTests)
import qualified TestInit (allTests)
import qualified TestMkPartition (allTests)
import qualified TestMultiOperations (allTests)
import qualified TestOpen (allTests)
import qualified TestOperations_Edit (allTests)
import qualified TestOperations_Setup (allTests)
import qualified TestOperations_ShellPartition (allTests)
import qualified TestOperations_Sync (allTests)
import qualified TestOperations_SyncEdit (allTests)
import qualified TestOperations_Up (allTests)
import qualified TestRepo (allTests)
import qualified TestSubstrateIO (allTests)
import qualified TestUdisksctl (allTests)

main :: IO ()
main = do
  results <- runTestTT allTests
  if failures results > 0 || errors results > 0
    then exitWith (ExitFailure 1)
    else return ()

allTests :: Test
allTests =
  TestList
    [ TestBase.allTests,
      TestClose.allTests,
      TestInit.allTests,
      TestMkPartition.allTests,
      TestMultiOperations.allTests,
      TestOpen.allTests,
      TestRepo.allTests,
      TestOperations_Setup.allTests,
      TestOperations_Edit.allTests,
      TestOperations_ShellPartition.allTests,
      TestOperations_Sync.allTests,
      TestOperations_SyncEdit.allTests,
      TestOperations_Up.allTests,
      TestSubstrateIO.allTests,
      TestUdisksctl.allTests
    ]
