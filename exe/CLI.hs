module CLI where

import qualified CLIHelp
import Options.Applicative
import System.FilePath.Posix
import Vaults.Operations

operationsParser ::
  ParserInfo Operation
operationsParser =
  info
    (operations <**> helper)
    ( fullDesc
        <> header "Vaults"
        <> progDesc "Create and manage encrypted, distributed, git-centric vault files."
    )

helpMod op = fullDesc <> header (CLIHelp.header op) <> progDesc (CLIHelp.progDesc op)

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

opInitVault = command "init" (info (opInitVaultParser <**> helper) (helpMod "init"))

opInitVaultParser =
  InitVault
    <$> argument str (metavar "VAULT_NAME")
    <*> argument str (metavar "LOCAL_PARTITION_NAME")

opMakePartition = command "mkpart" (info (opMakePartitionParser <**> helper) (helpMod "mkpart"))

opMakePartitionParser =
  MakePartition
    <$> argument str (metavar "PARTITION_NAME")
    <*> argument auto (metavar "PARTITION_SIZE")

opEditVault = command "edit" (info (opEditVaultParser <**> helper) (helpMod "edit"))

opEditVaultParser = pure EditVault

opShellVault = command "shell" (info (opShellVaultParser <**> helper) (helpMod "shell"))

opShellVaultParser = pure ShellVault

opShellPartition = command "shell-partition" (info (opShellPartitionParser <**> helper) (helpMod "shell-partition"))

opShellPartitionParser = ShellPartition <$> argument str (metavar "PARTITION")

opUploadVault = command "up" (info (opUploadVaultParser <**> helper) (helpMod "up"))

opUploadVaultParser = pure UploadVault

opUploadMultiVault = command "mup" (info (opUploadMultiVaultParser <**> helper) (helpMod "mup"))

opUploadMultiVaultParser = pure UploadMultiVault

opDownloadVault = command "down" (info (opDownloadVaultParser <**> helper) (helpMod "down"))

opDownloadVaultParser = pure DownloadVault

opDownloadMultiVault = command "mdown" (info (opDownloadMultiVaultParser <**> helper) (helpMod "mdown"))

opDownloadMultiVaultParser = pure DownloadMultiVault

opSyncVault = command "sync" (info (opSyncVaultParser <**> helper) (helpMod "sync"))

opSyncVaultParser = SyncVault <$> argument str (metavar "PARTITION")

opSyncEditVault = command "sync-edit" (info (opSyncEditVaultParser <**> helper) (helpMod "sync-edit"))

opSyncEditVaultParser = SyncEditVault <$> argument str (metavar "PARTITION")

opDiffLog = command "diff" (info (opDiffLogParser <**> helper) (helpMod "diff"))

opDiffLogParser = pure DiffLog

opDiffLogMultiVault = command "mdiff" (info (opDiffLogMultiVaultParser <**> helper) (helpMod "mdiff"))

opDiffLogMultiVaultParser = pure DiffLogMultiVault
