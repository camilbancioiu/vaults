module Assertions where

import Data.Maybe

import Test.HUnit
import MockSubstrate

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

assertOpError :: (Eq a, Show a) => String -> (Either String a, Mock) -> IO ()
assertOpError assertMsg (opResult, _) =
    assertEqual assertMsg (Left assertMsg) opResult

assertOpParamsError :: (Eq a, Show a) => String -> [String] -> Substrate.ExecResult -> (Either String a, Mock) -> IO ()
assertOpParamsError assertMsg params failedExec (opResult, _) =
    assertEqual assertMsg (Left errMsg) opResult
        where errMsg = (head params)
                       ++ " failed: "
                       ++ (Substrate.errorOutput failedExec)
                       ++ "\ncommand: "
                       ++ (show params)

assertNoExecCalls :: Mock -> IO ()
assertNoExecCalls mock =
    assertEqual "no exec calls" 0 (nExecs mock)
