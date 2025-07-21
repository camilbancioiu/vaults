module Assertions where

import Data.Foldable
import Data.Maybe
import MockSubstrate
import Test.HUnit
import qualified Vaults.Base as Base
import qualified Vaults.Substrate2 as Substrate

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
    ("execs not consumed:\n" ++ (show $ execResults mock))
    0
    (length $ execResults mock)

assertEqualLists ::
  (Eq a, Show a) =>
  String ->
  [a] ->
  [a] ->
  IO ()
assertEqualLists message expected actual = do
  let tuples = zip expected actual
  let tupleLines = map makeTupleLine tuples
  let diffStr = unlines tupleLines
  if expected /= actual
    then assertFailure (message ++ "\n" ++ diffStr)
    else return ()

  traverse_ (assertEqualInTuple message) tuples

makeTupleLine (expected, actual) =
  equality ++ (show expected) ++ equality ++ (show actual)
  where
    equality = if expected == actual then "\t==\t" else "\t=/=\t"

assertEqualInTuple ::
  (Eq a, Show a) =>
  String ->
  (a, a) ->
  Assertion
assertEqualInTuple message (expected, actual) = assertEqual message expected actual
