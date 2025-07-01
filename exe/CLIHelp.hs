module CLIHelp where

import qualified Vaults.MkPartition
import qualified Vaults.Operations as Operations

header "init" = "Initialize a vault"
header "mkpart" = "Create an encrypted partition"
header "edit" = "Unlock local partition and open editor"
header "shell" = "Unlock local partition and open a shell"
header "shell-partition" = "Unlock any partition and open a shell inside it"
header "up" = "Upload the local partition and its exported commit log"
header "mup" = "Upload local partitions of all the vaults in the current dir"
header "down" = "Download all the remote partitions of the vault"
header "mdown" = "Download all the remote partitions of all the vaults in the current dir"
header "sync" = "Unlock a remote partition and the local one and attempt to call git merge"
header "sync-edit" = "Unlock remote and local partitions, attempt to call git merge, then open editor"
header "diff" = "Display diffs of the local commit log and each of the remote commit logs"
header "mdiff" = ""

progDesc "init" =
  "Create a .vault directory in the current directory and\
  \ configure its name as VAULT_NAME. Also configure the name of the local\
  \ vault partition as LOCAL_PARTITION_NAME by writing it into .vault/local."
progDesc "mkpart" =
  "Create a new vault partition with the name\
  \ PARTITION_NAME.vault and size of PARTITION_SIZE megabytes. Minimum \
  \ allowed value for PARTITION_SIZE is "
    ++ (show Vaults.MkPartition.minPartitionSize)
    ++ ". If the vault is distributed across multiple machines, it is \
       \ recommended to create a vault partition on each machine with PARTITION_NAME \
       \ set to its hostname. It is also recommended to set PARTITION_SIZE equal for all \
       \ vault partitions, but not required."
progDesc "edit" =
  "Unlock and mount the local vault partition, change the working directory\
  \ to the 'repo' subdirectory inside it, then open the configured\
  \ editor. Closing the editor automatically exports the Git commit log and\
  \ locks the vault partition."
progDesc "shell" =
  "Unlock and mount the local vault partition, change the working directory\
  \ to the 'repo' subdirectory inside it, then open the a shell inside. Closing\
  \ the shell automatically locks the vault partition. The Git commit log is\
  \ not exported."
progDesc "shell-partition" =
  "Unlock and mount the vault partition PARTITION, change the working\
  \ directory to the 'repo' subdirectory inside it, then open the a shell\
  \ inside. Closing the shell automatically locks the vault partition. The Git\
  \ commit log is not exported."
progDesc "up" =
  "Upload the local vault partition and its exported Git commit\
  \ log to the location specified in .vault/remoteStore. No partition is unlocked."
progDesc "mup" =
  "Iterate over the vaults in the current directory and run the \"vault up\"\
  \ command for each of them. No partition is unlocked."
progDesc "down" =
  "Download all the remote vault partitions (listed in .vault/remotes) from the\
  \ location specified in .vault/remoteStore. No partition is unlocked."
progDesc "mdown" =
  "Iterate over the vaults in the current directory and run the \"vault down\"\
  \ command for each of them. No partition is unlocked."
progDesc "sync" =
  "Unlock and mount the remote vault partition PARTITION, then\
  \ unlock and mount the local partition, then fetch the mounted remote repository\
  \ into the local. Attempt to also merge the remote branch into the local. If it\
  \ fails, the fetch remains. Both partitions are locked immediately after the\
  \ merge attempt."
progDesc "sync-edit" =
  "Same as \"vault sync\", but also open the editor if the final merge was\
  \ successful."
progDesc "diff" =
  "Display a diff between the exported commit logs of the local\
  \ vault partition and each of the remote vault partitions (listed in\
  \ .vault/remotes). Will display as many diffs as there are remotes."
progDesc "mdiff" =
  "Iterate over the vaults in the current directory and run\
  \ the \"vault diff\" command for each of them. No partition is unlocked."
