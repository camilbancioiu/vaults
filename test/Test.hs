module Main (main) where

import System.Exit
import Test.HUnit

import qualified TestBase (allTests)
import qualified TestUdisksctl (allTests)
import qualified TestInit (allTests)
import qualified TestOpen (allTests)
import qualified TestClose (allTests)
import qualified TestOperations_Edit (allTests)
import qualified TestOperations_Sync (allTests)

main :: IO ()
main = do
    results <- runTestTT allTests
    if failures results > 0 || errors results > 0
       then exitWith (ExitFailure 1)
       else return ()

allTests :: Test
allTests = TestList [
      TestBase.allTests
    , TestUdisksctl.allTests
    , TestInit.allTests
    , TestOpen.allTests
    , TestClose.allTests
    , TestOperations_Edit.allTests
    , TestOperations_Sync.allTests
    ]
