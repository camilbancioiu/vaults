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
  let vaultDir = "/home/user/vaults/mockVault"
  let mock = emptyMock {currentDir = vaultDir}
  let result = runState (runExceptT $ Init.initVault "dummy" "localhost") mock
  let mockAfterExec = snd result

  assertEqual
    "dirs created by init"
    [".vault"]
    (createdDirs mockAfterExec)

  assertEqual
    "files created by init"
    [ (vaultDir, ".vault/name", "dummy"),
      (vaultDir, ".vault/local", "localhost"),
      (vaultDir, ".vault/remotes", ""),
      (vaultDir, ".vault/remoteStore", "")
    ]
    (writtenFiles mockAfterExec)
