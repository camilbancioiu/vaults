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

helpPrefix = "● "

helpMod :: String -> InfoMod a
helpMod op = fullDesc <> header (CLIHelp.header op) <> progDesc (helpPrefix ++ (CLIHelp.progDesc op))

operations =
  subparser $
    opInitVaultCommand
      <> opMakePartitionCommand
      <> opEditVaultCommand
      <> opShellVaultCommand
      <> opShellPartitionCommand
      <> opUploadVaultCommand
      <> opUploadMultiVaultCommand
      <> opDownloadVaultCommand
      <> opDownloadMultiVaultCommand
      <> opSyncVaultCommand
      <> opSyncEditVaultCommand
      <> opDiffLogCommand
      <> opDiffLogMultiVaultCommand

opInitVaultCommand = command "init" (info (opInitVaultParser <**> helper) (helpMod "init"))

opMakePartitionCommand = command "mkpart" (info (opMakePartitionParser <**> helper) (helpMod "mkpart"))

opEditVaultCommand = command "edit" (info (opEditVaultParser <**> helper) (helpMod "edit"))

opShellVaultCommand = command "shell" (info (opShellVaultParser <**> helper) (helpMod "shell"))

opShellPartitionCommand = command "shell-partition" (info (opShellPartitionParser <**> helper) (helpMod "shell-partition"))

opUploadVaultCommand = command "up" (info (opUploadVaultParser <**> helper) (helpMod "up"))

opUploadMultiVaultCommand = command "mup" (info (opUploadMultiVaultParser <**> helper) (helpMod "mup"))

opDownloadVaultCommand = command "down" (info (opDownloadVaultParser <**> helper) (helpMod "down"))

opDownloadMultiVaultCommand = command "mdown" (info (opDownloadMultiVaultParser <**> helper) (helpMod "mdown"))

opSyncVaultCommand = command "sync" (info (opSyncVaultParser <**> helper) (helpMod "sync"))

opSyncEditVaultCommand = command "sync-edit" (info (opSyncEditVaultParser <**> helper) (helpMod "sync-edit"))

opDiffLogCommand = command "diff" (info (opDiffLogParser <**> helper) (helpMod "diff"))

opDiffLogMultiVaultCommand = command "mdiff" (info (opDiffLogMultiVaultParser <**> helper) (helpMod "mdiff"))

opInitVaultParser =
  InitVault
    <$> argument str (metavar "VAULT_NAME")
    <*> argument str (metavar "LOCAL_PARTITION_NAME")

opMakePartitionParser =
  MakePartition
    <$> argument str (metavar "PARTITION_NAME")
    <*> argument auto (metavar "PARTITION_SIZE")

opEditVaultParser = pure EditVault

opShellVaultParser = pure ShellVault

opShellPartitionParser =
  ShellPartition
    <$> argument str (metavar "PARTITION")

opUploadVaultParser = pure UploadVault

opUploadMultiVaultParser = pure UploadMultiVault

opDownloadVaultParser = pure DownloadVault

opDownloadMultiVaultParser = pure DownloadMultiVault

opSyncVaultParser =
  SyncVault
    <$> argument str (metavar "PARTITION")

opSyncEditVaultParser =
  SyncEditVault
    <$> argument str (metavar "PARTITION")

opDiffLogParser = pure DiffLog

opDiffLogMultiVaultParser = pure DiffLogMultiVault
