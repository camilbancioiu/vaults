module Main where

import qualified Vaults (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Vaults.someFunc
