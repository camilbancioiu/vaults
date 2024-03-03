module Main where

import Control.Monad.Except
import System.Process
import Options.Applicative

import Debug.Trace
import CLI
import SubstrateIO

import Vaults.Base
import qualified Vaults.Substrate as Substrate
import Vaults.OpOpenVault
import Vaults.OpCloseVault

main :: IO ()
main = do
    vi <- loadVaultInfo
    let fname = (localname vi) ++ ".vault"
    let forced = False
    let params = ParamsOpenVault fname forced

    result <- runExceptT $ doVaultOp params
    case result of
         Left errMsg -> putStrLn errMsg
         Right ()    -> return ()

doVaultOp :: Substrate.Substrate m => ParamsOpenVault -> ExceptT String m ()
doVaultOp params = do
    vri <- openVault params
    trace "running nvim" (return ())
    lift $ Substrate.call "nvim" ["."]
    trace "closed nvim" (return ())
    closeVault vri
