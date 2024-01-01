module Assertions where

import Data.Maybe

import Test.HUnit
import MockSubstrate

import qualified Vaults.Base as V
import qualified Vaults.Substrate as VS

assertOpError :: (Eq a, Show a) => String -> (Either String a, Mock) -> IO ()
assertOpError assertMsg (opResult, _) =
    assertEqual assertMsg (Left assertMsg) opResult

assertOpParamsError :: (Eq a, Show a) => String -> [String] -> VS.ExecResult -> (Either String a, Mock) -> IO ()
assertOpParamsError assertMsg params failedExec (opResult, _) =
    assertEqual assertMsg (Left errMsg) opResult
        where errMsg = (head params)
                       ++ " failed: "
                       ++ (VS.errorOutput failedExec)
                       ++ "\ncommand: "
                       ++ (show params)

assertNoExecCalls :: (V.OpResult, Mock) -> IO ()
assertNoExecCalls (_, mock) =
    assertEqual "no exec calls" 0 (nExecs mock)

assertNoVaultEnvVar :: Mock -> IO ()
assertNoVaultEnvVar mock =
    assertEqual "no vault env var"
        Nothing
        (lookup key $ envVars mock)
    where key = V.activeVaultEnvName

assertVaultEnvVarSet :: Mock -> IO ()
assertVaultEnvVarSet mock =
    assertBool "vault env var set" (isJust mEnvVar)
    where mEnvVar = (lookup key $ envVars mock)
          key = V.activeVaultEnvName

assertActiveVaultEnvVarSet :: V.VaultRuntimeInfo -> Mock -> IO ()
assertActiveVaultEnvVarSet vri mock = do
    let key = V.activeVaultEnvName
    let mEnvVar = (lookup key $ envVars mock)
    case mEnvVar of
         Nothing -> assertFailure "no vault env var"
         Just envVar -> assertEqual "active vault as expected" vri (read envVar)
