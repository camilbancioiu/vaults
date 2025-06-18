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
      let vaultname = "mockVault"
      let hostname = "local"
      let partitionFile = "local.vault"
      let filesystemLabel = vaultname ++ "-" ++ hostname
      let mountpoint = "/dev/mapper/" ++ filesystemLabel

      let operation = Operations.doMakePartition hostname 64 mockVaultInfo
      let mock = addMockExecResult result mockWithVaultDir
            where
              result =
                Substrate.ExecResult
                  { Substrate.exitCode = ExitSuccess,
                    Substrate.output = hostname,
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
                  partitionFile
                ]
              ),
              ( "sudo",
                [ "cryptsetup",
                  "open",
                  "--type",
                  "luks",
                  partitionFile,
                  filesystemLabel
                ]
              ),
              ( "sudo",
                [ "mkfs.ext4",
                  "-L",
                  filesystemLabel,
                  mountpoint
                ]
              ),
              ("delay", []),
              ( "sudo",
                [ "cryptsetup",
                  "close",
                  filesystemLabel
                ]
              )
            ]
      assertEqual
        "expected commands"
        expectedCommands
        (execRecorded mockAfterExec)
