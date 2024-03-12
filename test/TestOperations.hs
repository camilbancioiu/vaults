module TestOperations where

import Control.Monad.State
import Control.Monad.Except

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Operations as Operations

import Test.HUnit
import Assertions
import MockSubstrate

allTests :: Test
allTests = TestList [
      test_editSuccessful
    , test_syncSuccessful
    ]

test_editSuccessful :: Test
test_editSuccessful =
    TestLabel "edit successful" $
    TestCase $ do
        let operation = Operations.doEditVault mockVaultInfo
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results = openVaultOk ++ closeVaultOk
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result
        assertEqual "vault opened, edited, closed" (Right()) (fst result)
        assertEqual "currentDir returns to srcDir"
            "/home/user"
            (currentDir mockAfterExec)

test_syncSuccessful :: Test
test_syncSuccessful =
    TestLabel "sync successful" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results =  openVaultOk
                                 ++ openVaultOk
                                 ++ closeVaultOk
                                 ++ closeVaultOk
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result
        assertEqual "vaults opened, local fetched remoteA, vaults closed" (Right()) (fst result)
        return ()
        -- assertFailure "not implemented"
