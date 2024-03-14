module TestOperations where

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
      test_editSuccessful
    -- , test_syncSuccessful
    ]

-- TODO test where the editor crashes
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

        -- no need to assert on a call to cd (cd needs a shell anyway); working
        -- dir is changed via Substrate.changeDir
        let expectedCmdsAppl = D.openPartitionCmds
                               ++ [ D.editCmd, D.gitLogCmd ]
                               ++ D.closePartitionCmds

        let expectedCommands = expectedCmdsAppl <*> (pure D.localOp)

        assertEqual "all commands executed"
            expectedCommands
            (execRecorded mockAfterExec)

-- TODO test where git crashes
test_syncSuccessful :: Test
test_syncSuccessful =
    TestLabel "sync successful" $
    TestCase $ do
        let operation = Operations.doSyncVault mockVaultInfo "remoteA"
        let mock = addMockExecResults results mockWithVaultAndRepoDir
                   where results =  openVaultOkRemote
                                 ++ openVaultOk
                                 ++ closeVaultOk
                                 ++ closeVaultOkRemote
        let result = runState (runExceptT $ operation) mock
        let mockAfterExec = snd result
        assertEqual "vaults opened, local fetched remoteA, vaults closed" (Right()) (fst result)
        -- TODO add assertion on expected commands
