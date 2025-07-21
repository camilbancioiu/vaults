module TestInit where

import Control.Monad.Except
import Control.Monad.State
import MockSubstrate
import Test.HUnit
import qualified Vaults.Init as Init

allTests :: Test
allTests =
  TestList
    [ test_init
    ]

test_init :: Test
test_init = TestCase $ do
  let operation = Init.initVault "dummy" "localhost"
  let mock = emptyMock {currentDir = mockVaultSourceDir}
  let operationResult = runState (runExceptT operation) mock
  let mockAfterExec = snd operationResult

  assertEqual
    "dirs created by init"
    [".vault"]
    (createdDirs mockAfterExec)

  assertEqual
    "files created by init"
    [ (mockVaultSourceDir, ".vault/name", "dummy"),
      (mockVaultSourceDir, ".vault/local", "localhost"),
      (mockVaultSourceDir, ".vault/remotes", ""),
      (mockVaultSourceDir, ".vault/remoteStore", "")
    ]
    (writtenFiles mockAfterExec)
