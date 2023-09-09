module Main (main) where

import qualified TestVaultBasics (allTests)
import qualified TestVaultOpen (allTests)

import System.Exit
import Test.HUnit

main :: IO ()
main = do
    results <- runTestTT allTests
    if failures results > 0 || errors results > 0
       then exitWith (ExitFailure 1)
       else return ()

allTests :: Test
allTests = TestList [
    TestVaultBasics.allTests,
    TestVaultOpen.allTests
    ]
