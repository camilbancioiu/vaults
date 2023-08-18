module Main (main) where

import System.Exit
import Test.HUnit

import qualified Vaults

main :: IO ()
main = do
    results <- runTestTT allTests
    if failures results > 0 || errors results > 0
       then exitWith (ExitFailure 1)
       else return ()

allTests :: Test
allTests = TestList $ concat [ [ testSubstrate ] ]

testSubstrate :: Test
testSubstrate = TestCase $ do
    v <- Vaults.readVault mockSubstrate
    assertEqual "substrate reading" "i'm just pretending" (Vaults.name v)
    putStrLn $ show v

realSubstrate = Vaults.Substrate {
    Vaults.subReadFile = Prelude.readFile
}

mockSubstrate = Vaults.Substrate {
    Vaults.subReadFile = mockReadFile
}

mockReadFile :: FilePath -> IO String
mockReadFile _ = return "i'm just pretending"
