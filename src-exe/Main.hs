module Main where

import Options.Applicative
import CLI
import SubstrateIO

import Vaults.OpOpenVault
import Vaults.OpCloseVault

main :: IO ()
main = do
  operation <- execParser operationsParser
  putStrLn (show operation)

  result <- case operation of
    OpenVault fname forced -> openVault (ParamsOpenVault fname forced)
    CloseVault             -> closeVault

  putStrLn (show result)
