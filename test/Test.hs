module Main (main) where

import System.Exit
import Test.HUnit

import qualified TestBase (allTests)
import qualified TestUdisksctl (allTests)
import qualified TestOpOpenVault (allTests)
import qualified TestOpCloseVault (allTests)

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
    , TestOpOpenVault.allTests
    , TestOpCloseVault.allTests
    ]
