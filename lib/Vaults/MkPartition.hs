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

  hostname <- getHostname
  owningUser <- getUsername
  owningGroup <- getGroupname

  let filesystemLabel = vaultName ++ "-" ++ hostname
  let mapperDev = "/dev/mapper/" ++ filesystemLabel
  ExceptT $
    Substrate.call
      "dd"
      [ "bs=1M",
        "count=" ++ (show partitionSize),
        "if=/dev/urandom",
        "of=" ++ partitionFilename
      ]
  ExceptT $
    Substrate.call
      "sudo"
      [ "cryptsetup",
        "--verify-passphrase",
        "luksFormat",
        partitionFilename
      ]
  ExceptT $
    Substrate.call
      "sudo"
      [ "cryptsetup",
        "open",
        "--type",
        "luks",
        partitionFilename,
        filesystemLabel
      ]
  ExceptT $
    Substrate.call
      "sudo"
      [ "mkfs.ext4",
        "-L",
        filesystemLabel,
        mapperDev
      ]

  mountpoint <- Udisksctl.mountDevice mapperDev

  ExceptT $
    Substrate.call
      "sudo"
      [ "chown",
        "-R",
        owningUser,
        mountpoint
      ]
  ExceptT $
    Substrate.call
      "sudo"
      [ "chgrp",
        "-R",
        owningGroup,
        mountpoint
      ]

  Udisksctl.unmountDevice mapperDev

  lift $ Substrate.delay 2000000
  ExceptT $
    Substrate.call
      "sudo"
      [ "cryptsetup",
        "close",
        filesystemLabel
      ]

getHostname ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getHostname = do
  result <- lift $ Substrate.exec "hostname" [] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get hostname")
  let hostname = Substrate.output result
  return (Base.stripTrailingNewline hostname)

getUsername ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getUsername = do
  result <- lift $ Substrate.exec "id" ["--user", "--name"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get username")
  let username = Substrate.output result
  return (Base.stripTrailingNewline username)

getGroupname ::
  (Substrate.Substrate m) =>
  ExceptT String m String
getGroupname = do
  result <- lift $ Substrate.exec "id" ["--group", "--name"] ""
  when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get groupname")
  let groupname = Substrate.output result
  return (Base.stripTrailingNewline groupname)
