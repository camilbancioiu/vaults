module Vaults.OperationParams where

data OpenVault = OpenVault (Maybe FilePath) Bool
               deriving (Eq, Show)
