module CLI where

import Options.Applicative
import System.FilePath.Posix

data Operation
  = InitVault String String
  | MakePartition String Int
  | EditVault
  | ShellVault
  | ShellPartition String
  | UploadVault
  | UploadMultiVault
  | DownloadVault
  | DownloadMultiVault
  | SyncVault String
  | SyncEditVault String
  | DiffLog
  | DiffLogMultiVault
  deriving (Show)

operationsParser :: ParserInfo Operation
operationsParser = info operations (progDesc "operation")

operations =
  subparser $
    opInitVault
      <> opMakePartition
      <> opEditVault
      <> opShellVault
      <> opShellPartition
      <> opUploadVault
      <> opUploadMultiVault
      <> opDownloadVault
      <> opDownloadMultiVault
      <> opSyncVault
      <> opSyncEditVault
      <> opDiffLog
      <> opDiffLogMultiVault

opInitVault = command "init" (info opInitVaultParser (progDesc "init vault"))

opInitVaultParser =
  InitVault
    <$> argument str (metavar "VAULT_NAME")
    <*> argument str (metavar "LOCAL_PARTITION_NAME")

opMakePartition = command "mkpart" (info opMakePartitionParser (progDesc "make partition"))

opMakePartitionParser =
  MakePartition
    <$> argument str (metavar "PARTITION_NAME")
    <*> argument auto (metavar "PARTITION_SIZE")

opEditVault = command "edit" (info opEditVaultParser (progDesc "edit vault"))

opEditVaultParser = pure EditVault

opShellVault = command "shell" (info opShellVaultParser (progDesc "edit vault"))

opShellVaultParser = pure ShellVault

opShellPartition = command "shell-partition" (info opShellPartitionParser (progDesc "edit partition"))

opShellPartitionParser = ShellPartition <$> argument str (metavar "PARTITION")

opUploadVault = command "up" (info opUploadVaultParser (progDesc "upload vault"))

opUploadVaultParser = pure UploadVault

opUploadMultiVault = command "mup" (info opUploadMultiVaultParser (progDesc "upload multiple vaults"))

opUploadMultiVaultParser = pure UploadMultiVault

opDownloadVault = command "down" (info opDownloadVaultParser (progDesc "download vault"))

opDownloadVaultParser = pure DownloadVault

opDownloadMultiVault = command "mdown" (info opDownloadMultiVaultParser (progDesc "download multiple vaults"))

opDownloadMultiVaultParser = pure DownloadMultiVault

opSyncVault = command "sync" (info opSyncVaultParser (progDesc "sync vault"))

opSyncVaultParser = SyncVault <$> argument str (metavar "PARTITION")

opSyncEditVault = command "sync-edit" (info opSyncEditVaultParser (progDesc "sync-edit vault"))

opSyncEditVaultParser = SyncEditVault <$> argument str (metavar "PARTITION")

opDiffLog = command "diff" (info opDiffLogParser (progDesc "diff vault log"))

opDiffLogParser = pure DiffLog

opDiffLogMultiVault = command "mdiff" (info opDiffLogMultiVaultParser (progDesc "diff logs of multiple vaults"))

opDiffLogMultiVaultParser = pure DiffLogMultiVault
