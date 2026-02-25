module Vaults.MkPartition where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as Udisksctl

minPartitionSize = 64

-- TODO upon failure, delete the created partition
-- TODO refactor, function far too long

makePartition ::
  (Substrate.Substrate m) =>
  String ->
  Int ->
  Base.VaultInfo ->
  ExceptT String m ()
makePartition partition partitionSize vi = do
  when
    (length partition == 0)
    (throwError "partition filename is required")

  when
    (partitionSize < 64)
    (throwError $ "partition size must be greater or equal to " ++ (show minPartitionSize))

  let partitionFilename = partition ++ ".vault"
  let vaultName = Base.name vi

  owningUser <- Base.getUsername
  owningGroup <- Base.getGroupname

  let filesystemLabel = vaultName ++ "-" ++ partition
  let mapperDev = "/dev/mapper/" ++ filesystemLabel

  Substrate.call
    "dd"
    [ "bs=1M",
      "count=" ++ (show partitionSize),
      "if=/dev/urandom",
      "of=" ++ partitionFilename
    ]
  Substrate.echo "Created randomness-filled partition file."

  Substrate.echo
    "Creating encrypted partition..."

  Substrate.call
    "sudo"
    [ "cryptsetup",
      "--verify-passphrase",
      "luksFormat",
      partitionFilename
    ]

  Substrate.echo
    "Opening encrypted partition..."

  Substrate.call
    "sudo"
    [ "cryptsetup",
      "open",
      "--type",
      "luks",
      partitionFilename,
      filesystemLabel
    ]

  Substrate.echo
    "Creating EXT4 filesystem inside encrypted partition..."

  Substrate.call
    "sudo"
    [ "mkfs.ext4",
      "-L",
      filesystemLabel,
      mapperDev
    ]

  mountpoint <- Udisksctl.mountDevice mapperDev
  Substrate.echo
    "EXT4 filesystem mounted."

  Substrate.call
    "sudo"
    [ "chown",
      "-R",
      owningUser,
      mountpoint
    ]

  Substrate.echo $
    "Set owner "
      ++ owningUser
      ++ "."

  Substrate.call
    "sudo"
    [ "chgrp",
      "-R",
      owningGroup,
      mountpoint
    ]

  Substrate.echo $ "Set group " ++ owningGroup ++ "."

  Udisksctl.unmountDevice mapperDev
  Substrate.echo "Unmounted."

  Substrate.delay
    2000000

  Substrate.call
    "sudo"
    [ "cryptsetup",
      "close",
      filesystemLabel
    ]

  Substrate.echo "Partition locked. Complete."
