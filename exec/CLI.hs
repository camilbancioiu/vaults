module CLI where

import System.FilePath.Posix
import Options.Applicative

data Operation = EditVault
               | ShellVault
               | UploadVault
               | DownloadVault
               | SyncVault String
               | DiffLog String
               deriving Show

operationsParser :: ParserInfo Operation
operationsParser = info operations (progDesc "operation")

operations = subparser $  opEditVault
                       <> opShellVault
                       <> opUploadVault
                       <> opDownloadVault
                       <> opSyncVault
                       <> opDiffLog

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
