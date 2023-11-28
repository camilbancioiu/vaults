module CLI where

import System.FilePath.Posix
import Options.Applicative

data Operation = OpenVault FilePath
               | CloseVault
               deriving Show

operationsParser :: ParserInfo Operation
operationsParser = info operations (progDesc "operation")

operations = subparser $ opOpenVault <> opCloseVault

opOpenVault = command "open" (info opOpenVaultParser (progDesc "open vault"))
opOpenVaultParser = OpenVault <$> argument str (metavar "FILE")

opCloseVault = command "close" (info opCloseVaultParser (progDesc "close vault"))
opCloseVaultParser = pure CloseVault
