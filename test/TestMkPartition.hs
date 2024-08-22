module TestMkPartition where

import Assertions
import Control.Monad.Except
import Control.Monad.State
import qualified DummyValues as D
import MockSubstrate
import System.Exit
import Test.HUnit
import qualified Vaults.Base as Base
import qualified Vaults.Operations as Operations
import qualified Vaults.Substrate as Substrate

allTests :: Test
allTests =
  TestList
    [ test_MkPartitionSuccess
    ]

test_MkPartitionSuccess :: Test
test_MkPartitionSuccess =
  TestLabel "make partition succeeds" $
    TestCase $ do
      let operation = Operations.doMakePartition "local" 64 mockVaultInfo
      let mock = addMockExecResult result mockWithVaultDir
            where
              result =
                Substrate.ExecResult
                  { Substrate.exitCode = ExitSuccess,
                    Substrate.output = "local",
                    Substrate.errorOutput = ""
                  }
      let result = runState (runExceptT $ operation) mock
      let mockAfterExec = snd result
      assertEqual "" (Right ()) (fst result)

      let expectedCommands =
            [ ("hostname", []),
              ( "dd",
                [ "bs=1M",
                  "count=64",
                  "if=/dev/urandom",
                  "of=local.vault"
                ]
              ),
              ( "sudo",
                [ "cryptsetup",
                  "--verify-passphrase",
                  "luksFormat",
                  "local.vault"
                ]
              ),
              ( "sudo",
                [ "cryptsetup",
                  "open",
                  "--type",
                  "luks",
                  "local.vault",
                  "mockVault"
                ]
              ),
              ( "sudo",
                [ "mkfs.ext4",
                  "-L",
                  "mockVault-local",
                  "/dev/mapper/mockVault"
                ]
              ),
              ("delay", []),
              ( "sudo",
                [ "cryptsetup",
                  "close",
                  "mockVault"
                ]
              )
            ]
      assertEqual
        "expected commands"
        expectedCommands
        (execRecorded mockAfterExec)
