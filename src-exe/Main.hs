module Main where

import Options.Applicative
import CLI
import Vaults.OpOpenVault
import Vaults.OpCloseVault

main :: IO ()
main = do
  operation <- execParser operationsParser
  putStrLn (show operation)
