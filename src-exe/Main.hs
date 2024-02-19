module Main where

import System.Process
import Options.Applicative
import CLI
import SubstrateIO

import Vaults.Base
import Vaults.OpOpenVault
import Vaults.OpCloseVault

main :: IO ()
main = do
    vi <- loadVaultInfo
    let fname = (localname vi) ++ ".vault"
    let forced = False
    result <- openVault (ParamsOpenVault fname forced)
    putStrLn (show result)
    case result of
         Left errMsg -> return ()
         Right vri   -> do
             callProcess "nvim" ["."]
             result <- closeVault
             case result of
                Left errMsg -> putStrLn errMsg
                Right ()    -> return ()
