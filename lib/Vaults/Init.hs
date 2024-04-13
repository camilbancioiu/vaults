module Vaults.Init where

initVault :: Substrate.Substrate m => String -> String -> m ()
initVault vaultName localName = do
    return ()
