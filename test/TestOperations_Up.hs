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

      let expectedCommands = D.uploadPartitionCmds "local"

      let mock = mockWithVaultAndRepoDir
      let operationResult = runState (runExceptT operation) mock
      let mockAfterExec = snd operationResult

      assertEqual
        "uploaded single vault successfuly"
        (Right ())
        (fst operationResult)

      assertEqual
        "commands"
        expectedCommands
        (execRecorded mockAfterExec)
