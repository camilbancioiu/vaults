module CLI where

import System.FilePath.Posix
import Options.Applicative

data Operation = OpenVault FilePath
               | CloseVault
               deriving Show

operationsParser :: ParserInfo Operation
operationsParser = info ops (progDesc "operation")
                   where ops = subparser $ opOpenVault <> opCloseVault
                         opOpenVault = command "open" (info opOpenVaultParser (progDesc "open vault"))
                         opCloseVault = command "close" (info opCloseVaultParser (progDesc "close vault"))
                         opOpenVaultParser = OpenVault <$> argument str (metavar "FILE")
                         opCloseVaultParser = pure CloseVault
