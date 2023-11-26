module Main where

import Options.Applicative
import CLI

-- vault open partition.vault
-- vault close

main :: IO ()
main = do
  operation <- execParser operationsParser
  putStrLn (show operation)
