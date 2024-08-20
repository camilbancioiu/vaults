module TestOperations_Up where

import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import Test.HUnit
import qualified Vaults.Operations as Operations

allTests :: Test
allTests =
  TestList
    [ testSingleUpload_success
    ]

testSingleUpload_success :: Test
testSingleUpload_success =
  TestLabel "upload single vault successfuly" $
    TestCase $ do
      let operation = Operations.doUploadVault mockVaultInfo
      let mock = addMockExecResults results mockWithVaultAndRepoDir
            where
              results = []

      let (result, mockAfterExec) = runState (runExceptT $ operation) mock

      assertEqual "uploaded single vault successfuly" (Right ()) result

      let expectedCommands = D.uploadPartitionCmds "local"
      assertEqual "commands" expectedCommands (execRecorded mockAfterExec)
