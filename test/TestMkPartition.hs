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
      let partitionName = "local"
      let partitionFile = "local.vault"
      let filesystemLabel = vaultname ++ "-" ++ partitionName

      let owningUser = "theUser"
      let owningGroup = "groupOfTheUser"

      let operation = Operations.doMakePartition partitionName 64 mockVaultInfo
      let mock = addMockExecResults results mockWithVaultDir
            where
              results =
                [ Substrate.ExecResult
                    { Substrate.exitCode = ExitSuccess,
                      Substrate.output = owningUser,
                      Substrate.errorOutput = ""
                    },
                  Substrate.ExecResult
                    { Substrate.exitCode = ExitSuccess,
                      Substrate.output = owningGroup,
                      Substrate.errorOutput = ""
                    },
                  D.mountExec True D.localOp,
                  D.unmountExec True D.localOp
                ]

      let result = runState (runExceptT $ operation) mock
      let mockAfterExec = snd result
      assertEqual "" (Right ()) (fst result)

      let expectedCommands =
            [ ("id", ["--user", "--name"]),
              ("id", ["--group", "--name"]),
              ( "dd",
                [ "bs=1M",
                  "count=64",
                  "if=/dev/urandom",
                  "of=" ++ partitionFile
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
                  D.mapperDev D.localOp
                ]
              ),
              D.mountCmd D.localOp,
              ("sudo", ["chown", "-R", owningUser, D.mountpoint D.localOp]),
              ("sudo", ["chgrp", "-R", owningGroup, D.mountpoint D.localOp]),
              D.unmountCmd D.localOp,
              ("delay", []),
              ("sudo", ["cryptsetup", "close", filesystemLabel])
            ]
      assertEqualLists
        "expected commands"
        expectedCommands
        (execRecorded mockAfterExec)
