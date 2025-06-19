module Vaults.MkPartition where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import System.Exit
import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate
import qualified Vaults.Udisksctl as Udisksctl

-- TODO upon failure, delete the created partition

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

  let partitionFilename = partition ++ ".vault"
  let vaultName = Base.name vi

  hostname <- Base.getHostname
  owningUser <- Base.getUsername
  owningGroup <- Base.getGroupname

  let filesystemLabel = vaultName ++ "-" ++ hostname
  let mapperDev = "/dev/mapper/" ++ filesystemLabel

  lift $
    Substrate.call
      "dd"
      [ "bs=1M",
        "count=" ++ (show partitionSize),
        "if=/dev/urandom",
        "of=" ++ partitionFilename
      ]
  lift $ Substrate.echo "Created randomness-filled partition file."

  lift $ Substrate.echo "Creating encrypted partition..."
  lift $
    Substrate.call
      "sudo"
      [ "cryptsetup",
        "--verify-passphrase",
        "luksFormat",
        partitionFilename
      ]

  lift $ Substrate.echo "Opening encrypted partition..."
  lift $
    Substrate.call
      "sudo"
      [ "cryptsetup",
        "open",
        "--type",
        "luks",
        partitionFilename,
        filesystemLabel
      ]

  lift $ Substrate.echo "Creating EXT4 filesystem inside encrypted partition..."
  lift $
    Substrate.call
      "sudo"
      [ "mkfs.ext4",
        "-L",
        filesystemLabel,
        mapperDev
      ]

  mountpoint <- Udisksctl.mountDevice mapperDev
  lift $ Substrate.echo "EXT4 filesystem mounted."

  lift $
    Substrate.call
      "sudo"
      [ "chown",
        "-R",
        owningUser,
        mountpoint
      ]
  lift $ Substrate.echo $ "Set owner " ++ owningUser ++ "."
  lift $
    Substrate.call
      "sudo"
      [ "chgrp",
        "-R",
        owningGroup,
        mountpoint
      ]
  lift $ Substrate.echo $ "Set group " ++ owningGroup ++ "."

  Udisksctl.unmountDevice mapperDev
  lift $ Substrate.echo "Unmounted."

  lift $ Substrate.delay 2000000

  lift $
    Substrate.call
      "sudo"
      [ "cryptsetup",
        "close",
        filesystemLabel
      ]

  lift $ Substrate.echo "Partition locked. Complete."
