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
        assertEqual "all commands executed"
            [ ("udisksctl", ["loop-setup", "-f", "local.vault"])
            , ("udisksctl", ["unlock", "-b", "/dev/loop42"])
            , ("udisksctl", ["mount", "-b", "/dev/dm-4"])
            -- no need for cd; working dir is changed via Substrate.changeDir
            , ("nvim", ["."]
            , ("udisksctl", ["unmount", "-b", "/dev/dm-4"])
            , ("udisksctl", ["lock", "-b", "/dev/loop42"])
            , ("udisksctl", ["loop-delete", "-b", "/dev/loop42"])
            ]
            (execRecorded mockAfterExec)

-- TODO test where git crashes
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
