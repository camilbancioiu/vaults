module TestBase where

import Test.HUnit

import Data.Maybe
import Control.Monad.State

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import MockSubstrate

allTests :: Test
allTests = TestList [
      test_isVaultDir
    , test_loadVaultInfo
    , test_isAnyVaultActive
    , test_getActiveVault
    , test_setActiveVault
    , test_unsetActiveVault
    , test_getPartitionLocation
    ]


test_isVaultDir :: Test
test_isVaultDir = TestCase $ do
    let mock = emptyMock
    let isV = evalState Base.isVaultDir mock
    assertEqual "isVaultDir" False isV

    let mock = mockWithVaultDir
    let isV = evalState Base.isVaultDir mock
    assertEqual "isVaultDir" True isV


-- TODO test when files contain trailing newlines
test_loadVaultInfo :: Test
test_loadVaultInfo = TestCase $ do
    let mock = mockWithVaultDir
    let expected = mockVaultInfo
    let vi = evalState Base.loadVaultInfo mock
    assertEqual "loadVaultInfo" expected vi


test_isAnyVaultActive :: Test
test_isAnyVaultActive = TestCase $ do
    let mock = emptyMock
    let active = evalState Base.isAnyVaultActive mock
    assertBool "no vault active" (not active)

    let var = (Base.activeVaultEnvName, "something")
    let mock = mockWithEnvVar var
    let active = evalState Base.isAnyVaultActive mock
    assertBool "vault is active" active


test_getActiveVault :: Test
test_getActiveVault = TestCase $ do
    let mock = emptyMock
    let vri = evalState Base.getActiveVault mock
    assertEqual "no vault active" Nothing vri

    let mockVRI = mockVaultRuntimeInfo
    let var = (Base.activeVaultEnvName, show mockVRI)
    let mock = mockWithEnvVar var
    let vri = evalState Base.getActiveVault mock
    assertEqual "loaded vault" (Just mockVRI) vri


test_setActiveVault :: Test
test_setActiveVault = TestCase $ do
    let mock = emptyMock
    let mockVRI = mockVaultRuntimeInfo
    let setget = (Base.setActiveVault mockVRI >> Base.getActiveVault)
    let vri = evalState setget mock
    assertEqual "set and get active vault" (Just mockVRI) vri


test_unsetActiveVault :: Test
test_unsetActiveVault = TestCase $ do
    let var = (Base.activeVaultEnvName, "something")
    let mock = mockWithEnvVar var
    let active = evalState Base.isAnyVaultActive mock
    assertBool "vault is active" active

    let active = evalState (Base.unsetActiveVault >> Base.isAnyVaultActive) mock
    assertBool "vault is unset" (not active)


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
