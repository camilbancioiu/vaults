module Vaults.MkPartition where

import Control.Monad.Except

import Vaults.Base
import qualified Vaults.Substrate as Substrate

-- dd if=/dev/urandom of=$fname bs=1M count=$mb || { return 1; }
-- sudo cryptsetup --verify-passphrase luksFormat $fname || { return 1; }
-- sudo cryptsetup open --type luks $fname $vaultname || { return 1; }
-- sudo mkfs.ext4 -L $fslabel /dev/mapper/$vaultname || { return 1; }
-- sleep 2
-- sudo cryptsetup close $vaultname || { return 1; }

makePartition :: Substrate.Substrate m => String -> Int -> VaultInfo -> ExceptT String m ()
makePartition partition partitionSize vi = do
    when (length partition == 0)
         (throwError "partition filename is required")

    let partitionFilename = partition ++ ".vault"
    ExceptT $ Substrate.call "dd" [ "bs=1M"
                                  , "count=" ++ (show partitionSize)
                                  , "if=/dev/urandom"
                                  , "of=" ++ partitionFilename
                                  ]
    return ()
