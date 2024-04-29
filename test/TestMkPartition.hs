module TestMkPartition where

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Operations as Operations

import Test.HUnit
import Assertions
import MockSubstrate
import qualified DummyValues as D

allTests :: Test
allTests = TestList [
    -- test_MkPartitionSuccess
    ]

test_MkPartitionSuccess :: Test
test_MkPartitionSuccess =
    TestLabel "make partition succeeds" $
    TestCase $ do
        -- let operation =
        -- let mock = mockWithVaultDir
        assertFailure "not implemented"


