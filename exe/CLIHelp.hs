module CLIHelp where

import qualified Vaults.MkPartition
import qualified Vaults.Operations as Operations

header "init" = "Initialize a Vault"
header "mkpart" = "Create an encrypted partition"
header "edit" = ""
header "shell" = ""
header "shell-partition" = ""
header "up" = ""
header "mup" = ""
header "down" = ""
header "mdown" = ""
header "sync" = ""
header "sync-edit" = ""
header "diff" = ""
header "mdiff" = ""

progDesc "init" =
  "* Create a .vault directory in the current directory and\
  \ configure its name as VAULT_NAME. Also configure the name of the local\
  \ vault partition as LOCAL_PARTITION_NAME."
progDesc "mkpart" =
  "* Create a new vault partition with the name\
  \ PARTITION_NAME.vault and size of PARTITION_SIZE megabytes. Minimum \
  \ allowed value for PARTITION_SIZE is "
    ++ (show Vaults.MkPartition.minPartitionSize)
    ++ ". If the vault is distributed across multiple machines, it is \
       \ recommended to create a vault partition on each machine with PARTITION_NAME \
       \ set to its hostname. It is also recommended to set PARTITION_SIZE equal for all \
       \ vault partitions, but not required."
progDesc "edit" = ""
progDesc "shell" = ""
progDesc "shell-partition" = ""
progDesc "up" = ""
progDesc "mup" = ""
progDesc "down" = ""
progDesc "mdown" = ""
progDesc "sync" = ""
progDesc "sync-edit" = ""
progDesc "diff" = ""
progDesc "mdiff" = ""
