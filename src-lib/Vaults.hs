module Vaults where

data Substrate = Substrate {
    subReadFile :: (FilePath -> IO String)
}

data Vault = Vault {
    name :: String,
    localname :: String,
    remotes :: [String],
    remoteStore :: String
} deriving Show

readVault :: Substrate -> IO Vault
readVault s = do
    vname <- (subReadFile s) ".vault/name"
    vlocalname <- (subReadFile s) ".vault/local"
    vremotes <- (subReadFile s) ".vault/remotes"
    vremoteStore <- (subReadFile s) ".vault/remoteStore"

    return Vault {
        name = vname,
        localname = vlocalname,
        remotes = lines vremotes,
        remoteStore = vremoteStore
    }
