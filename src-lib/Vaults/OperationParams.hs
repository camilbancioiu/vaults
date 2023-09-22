module Vaults.OperationParams where

data OpenVault = OpenVault {
    partitionFilename :: Maybe FilePath,
    isForcedOpening :: Bool
} deriving (Eq, Show)
