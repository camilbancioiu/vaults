module TestBase where

import Control.Monad.Except
import Control.Monad.State
import Data.Maybe
import MockSubstrate
import Test.HUnit
import qualified Vaults.Base as Base

allTests :: Test
allTests =
  TestList
    [ test_isVaultDir,
      test_loadVaultInfo,
      test_getPartitionLocation,
      test_stripTrailingNewline
    ]

test_isVaultDir :: Test
test_isVaultDir = TestCase $ do
  let mock = emptyMock
  let isV = evalState (runExceptT Base.isVaultDir) mock
  assertEqual "isVaultDir" (Right False) isV

  let mock = mockWithVaultDir
  let isV = evalState (runExceptT Base.isVaultDir) mock
  assertEqual "isVaultDir" (Right True) isV

-- TODO test when files contain trailing newlines
test_loadVaultInfo :: Test
test_loadVaultInfo = TestCase $ do
  let mock = mockWithVaultDir
  let expected = mockVaultInfo
  let vi = evalState (runExceptT Base.loadVaultInfo) mock
  assertEqual "loadVaultInfo" (Right expected) vi

test_getPartitionLocation :: Test
test_getPartitionLocation = TestCase $ do
  let vi = mockVaultInfo
  Base.LocalPartition @=? Base.getPartitionLocation vi "local.vault"
  Base.UnknownPartition @=? Base.getPartitionLocation vi ""
  Base.UnknownPartition @=? Base.getPartitionLocation vi ".vault"
  Base.UnknownPartition @=? Base.getPartitionLocation vi ".vau"
  Base.UnknownPartition @=? Base.getPartitionLocation vi "local.vau"
  Base.UnknownPartition @=? Base.getPartitionLocation vi "rem.vault"
  Base.UnknownPartition @=? Base.getPartitionLocation vi "remote.vault"
  Base.UnknownPartition @=? Base.getPartitionLocation vi "remoteC.vault"
  Base.RemotePartition @=? Base.getPartitionLocation vi "remoteA.vault"
  Base.RemotePartition @=? Base.getPartitionLocation vi "remoteB.vault"

test_stripTrailingNewline :: Test
test_stripTrailingNewline = TestCase $ do
  "one two" @=? Base.stripTrailingNewline "one two\n"
  "one two" @=? Base.stripTrailingNewline "one two\n\n"
  "one two\nthree" @=? Base.stripTrailingNewline "one two\nthree\n"
