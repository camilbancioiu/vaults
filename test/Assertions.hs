module Assertions where

import Data.Foldable
import Data.Maybe
import MockSubstrate
import Test.HUnit
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

assertOpError ::
  (Eq a, Show a) =>
  String ->
  (Either String a, Mock) ->
  IO ()
assertOpError assertMsg (opResult, _) =
  assertEqual assertMsg (Left assertMsg) opResult

assertOpParamsError ::
  (Eq a, Show a) =>
  String ->
  [String] ->
  Substrate.ExecResult ->
  (Either String a, Mock) ->
  IO ()
assertOpParamsError assertMsg params failedExec (opResult, _) =
  assertEqual assertMsg (Left errMsg) opResult
  where
    errMsg =
      (head params)
        ++ " failed: "
        ++ (Substrate.errorOutput failedExec)
        ++ "\ncommand: "
        ++ (show params)

assertNoExecCalls ::
  Mock ->
  IO ()
assertNoExecCalls mock =
  assertEqual "no exec calls" 0 (nExecs mock)

assertAllExecsConsumed ::
  Mock ->
  IO ()
assertAllExecsConsumed mock =
  assertEqual
    "all execs consumed"
    0
    (length $ execResults mock)

assertEqualLists ::
  (Eq a, Show a) =>
  String ->
  [a] ->
  [a] ->
  IO ()
assertEqualLists message expected actual =
  let tuples = zip expected actual
   in traverse_ (assertEqualInTuple message) tuples

assertEqualInTuple ::
  (Eq a, Show a) =>
  String ->
  (a, a) ->
  Assertion
assertEqualInTuple message (expected, actual) = assertEqual message expected actual
