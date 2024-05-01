module CLI where

import System.FilePath.Posix
import Options.Applicative

data Operation = InitVault String String
               | MakePartition String Int
               | EditVault
               | ShellVault
               | UploadVault
               | DownloadVault
               | SyncVault String
               | DiffLog String
               deriving Show

operationsParser :: ParserInfo Operation
operationsParser = info operations (progDesc "operation")

operations = subparser $  opInitVault
                       <> opMakePartition
                       <> opEditVault
                       <> opShellVault
                       <> opUploadVault
                       <> opDownloadVault
                       <> opSyncVault
                       <> opDiffLog

opInitVault = command "init" (info opInitVaultParser (progDesc "init vault"))
opInitVaultParser = InitVault <$> argument str (metavar "VAULT_NAME")
                              <*> argument str (metavar "LOCAL_PARTITION_NAME")

opMakePartition = command "mkpart" (info opMakePartitionParser (progDesc "make partition"))
opMakePartitionParser = MakePartition <$> argument str (metavar "PARTITION_NAME")
                                      <*> argument auto (metavar "PARTITION_SIZE")

opEditVault = command "edit" (info opEditVaultParser (progDesc "edit vault"))
opEditVaultParser = pure EditVault

opShellVault = command "shell" (info opShellVaultParser (progDesc "edit vault"))
opShellVaultParser = pure ShellVault

opUploadVault = command "up" (info opUploadVaultParser (progDesc "upload vault"))
opUploadVaultParser = pure UploadVault

opDownloadVault = command "down" (info opDownloadVaultParser (progDesc "download vault"))
opDownloadVaultParser = pure DownloadVault

opSyncVault = command "sync" (info opSyncVaultParser (progDesc "sync vault"))
opSyncVaultParser = SyncVault <$> argument str (metavar "PARTITION")

opDiffLog = command "diff" (info opDiffLogParser (progDesc "diff vault log"))
opDiffLogParser = DiffLog <$> argument str (metavar "PARTITION")
