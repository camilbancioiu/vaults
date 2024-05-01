module Vaults.MkPartition where

import Control.Monad.Except
import System.Exit

import qualified Vaults.Base as Base
import qualified Vaults.Substrate as Substrate

-- local localhost=$(hostname)
-- local fname="$localhost.vault"
-- local fslabel="$vaultname-$localhost"
-- dd if=/dev/urandom of=$fname bs=1M count=$mb || { return 1; }
-- sudo cryptsetup --verify-passphrase luksFormat $fname || { return 1; }
-- sudo cryptsetup open --type luks $fname $vaultname || { return 1; }
-- sudo mkfs.ext4 -L $fslabel /dev/mapper/$vaultname || { return 1; }
-- sleep 2
-- sudo cryptsetup close $vaultname || { return 1; }
-- TODO upon failure, delete the created partition

makePartition :: Substrate.Substrate m => String -> Int -> Base.VaultInfo -> ExceptT String m ()
makePartition partition partitionSize vi = do
    when (length partition == 0)
         (throwError "partition filename is required")

    let partitionFilename = partition ++ ".vault"
    let vaultName = Base.name vi
    hostname <- getHostname
    let filesystemLabel = vaultName ++ "-" ++ hostname
    ExceptT $ Substrate.call "dd" [ "bs=1M"
                                  , "count=" ++ (show partitionSize)
                                  , "if=/dev/urandom"
                                  , "of=" ++ partitionFilename
                                  ]
    ExceptT $ Substrate.call "sudo" [ "cryptsetup"
                                    , "--verify-passphrase"
                                    , "luksFormat"
                                    , partitionFilename
                                    ]
    ExceptT $ Substrate.call "sudo" [ "cryptsetup"
                                    , "open"
                                    , "--type"
                                    , "luks"
                                    , partitionFilename
                                    , vaultName
                                    ]
    ExceptT $ Substrate.call "sudo" [ "mkfs.ext4"
                                    , "-L"
                                    , filesystemLabel
                                    , "/dev/mapper/" ++ vaultName
                                    ]
    lift $ Substrate.delay 2000000
    ExceptT $ Substrate.call "sudo" [ "cryptsetup"
                                    , "close"
                                    , vaultName
                                    ]

getHostname :: Substrate.Substrate m => ExceptT String m String
getHostname = do
    result <- lift $ Substrate.exec "hostname" [] ""
    when (Substrate.exitCode result /= ExitSuccess) (throwError "could not get hostname")
    let hostname = Substrate.output result
    return hostname
