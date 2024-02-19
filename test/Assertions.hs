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

assertNoVaultEnvVar :: Mock -> IO ()
assertNoVaultEnvVar mock =
    assertEqual "no vault env var"
        Nothing
        (lookup key $ envVars mock)
    where key = Base.activeVaultEnvName

assertVaultEnvVarSet :: Mock -> IO ()
assertVaultEnvVarSet mock =
    assertBool "vault env var set" (isJust mEnvVar)
    where mEnvVar = (lookup key $ envVars mock)
          key = Base.activeVaultEnvName

assertActiveVaultEnvVarSet :: Base.VaultRuntimeInfo -> Mock -> IO ()
assertActiveVaultEnvVarSet vri mock = do
    let key = Base.activeVaultEnvName
    let mEnvVar = (lookup key $ envVars mock)
    case mEnvVar of
         Nothing -> assertFailure "no vault env var"
         Just envVar -> assertEqual "active vault as expected" vri (read envVar)
