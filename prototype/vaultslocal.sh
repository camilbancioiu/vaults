vnvim () {
  local localname=$(cat .vault/local)
  local vaultfile="$localname.vault"
  vaultopen $vaultfile
  nvim .
  vaultclose
}

mkvault () {
  local vaultname=$1
  if [[ -z "$vaultname" ]]; then
    echo "vault name required"
    return 1
  fi

  local mb=$2
  if [[ -z "$mb" ]]; then
    echo "size in MB required"
    return 1
  fi

  local localhost=$(hostname)
  local fname="$localhost.vault"
  local fslabel="$vaultname-$localhost"
  dd if=/dev/urandom of=$fname bs=1M count=$mb || { return 1; }
  sudo cryptsetup --verify-passphrase luksFormat $fname || { return 1; }
  sudo cryptsetup open --type luks $fname $vaultname || { return 1; }
  sudo mkfs.ext4 -L $fslabel /dev/mapper/$vaultname || { return 1; }
  sleep 2
  sudo cryptsetup close $vaultname || { return 1; }
}

vaultopen () {
  guard_is_vault_srcdir || { return 1; }
  vaultopenf "$@"
}

vaultopenf () {
  guard_no_active_vault || { return 1; }

  local vaultfile=$1
  local location=$(guard_local_vaultopen $vaultfile)
  local force_rw=$2

  local mountopts=""
  if [[ "$location" == "remote" ]]; then
    echo "vault partition not local, mounting read-only..."
    mountopts="-o ro"
  fi
  if [[ "$force_rw" == "rw" ]]; then
    echo "forcibly mounting read-write..."
    mountopts=""
  fi

  local loopdev=$(createloop "$vaultfile") || { return 1; }
  local mountinfo=$(mountlockeddev "$loopdev" $mountopts) || {
    deleteloop "$loopdev";
    return 1;
  }

  set_active_vault "$loopdev" "$mountinfo" "$vaultfile" "$location"

  echo "vault opened"
  cd "$CURRENT_VAULT_DIR"
  export VIM_PRIVATE=1
}

vaultclose () {
  guard_active_vault || { return 1; }

  if [[ "$CURRENT_VAULT_LOCATION" == "local" ]]; then
    local localname=$(extract_vault_localname $CURRENT_VAULT_FILE)
    save_vault_log "$localname" "$CURRENT_VAULT_SRCDIR"
  fi

  cd "$CURRENT_VAULT_SRCDIR"
  sleep 2

  lockmounteddev "$CURRENT_VAULT_DM" "$CURRENT_VAULT_LOOP" || {
    deleteloop "$CURRENT_VAULT_LOOP";
    return 1;
  }

  deleteloop "$CURRENT_VAULT_LOOP"

  unset_active_vault
  echo "vault closed"
  export VIM_PRIVATE=0
}

vaultget () {
  echo "CURRENT_VAULT_SRCDIR=$CURRENT_VAULT_SRCDIR"
  echo "CURRENT_VAULT_LOOP=$CURRENT_VAULT_LOOP"
  echo "CURRENT_VAULT_DM=$CURRENT_VAULT_DM"
  echo "CURRENT_VAULT_DIR=$CURRENT_VAULT_DIR"
  echo "CURRENT_VAULT_FILE=$CURRENT_VAULT_FILE"
  echo "CURRENT_VAULT_LOCATION=$CURRENT_VAULT_LOCATION"
}

save_vault_log () {
  local localname=$1
  local vault_srcdir=$2

  local logfile="$vault_srcdir/$localname.log"

  git log --format=%H > $logfile
}

set_active_vault () {
  local loopdev=$1
  local mountinfo=$2
  local vaultfile=$3
  local location=$4

  mountdev=$(element "$mountinfo" 1)
  vaultdir=$(element "$mountinfo" 2)
  vaultname=$(cat .vault/name)

  export CURRENT_VAULT_SRCDIR="$(pwd)"
  export CURRENT_VAULT_LOOP="$loopdev"
  export CURRENT_VAULT_DM="$mountdev"
  # export CURRENT_VAULT_DIR="$vaultdir/repo"
  export CURRENT_VAULT_DIR="$vaultdir"
  export CURRENT_VAULT_FILE="$vaultfile"
  export CURRENT_VAULT_LOCATION="$location"
}

unset_active_vault () {
  export CURRENT_VAULT_SRCDIR=""
  export CURRENT_VAULT_LOOP=""
  export CURRENT_VAULT_DM=""
  export CURRENT_VAULT_DIR=""
  export CURRENT_VAULT_FILE=""
  export CURRENT_VAULT_LOCATION=""
}

guard_is_vault_srcdir () {
  if [[ ! -d ".vault" ]]; then
    echo "this is not a vault, aborting..."
    return 1
  fi
}

guard_no_active_vault () {
  if [[ -n "$CURRENT_VAULT_SRCDIR" ]]; then
    echo "a vault is already active, aborting..."
    return 1
  fi

  if [[ -n "$CURRENT_VAULT_LOOP" ]]; then
    echo "a vault is already active, aborting..."
    return 1
  fi

  if [[ -n "$CURRENT_VAULT_DM" ]]; then
    echo "a vault is already active, aborting..."
    return 1
  fi

  if [[ -n "$CURRENT_VAULT_DIR" ]]; then
    echo "a vault is already active, aborting..."
    return 1
  fi

  if [[ -n "$CURRENT_VAULT_FILE" ]]; then
    echo "a vault is already active, aborting..."
    return 1
  fi

  if [[ -n "$CURRENT_VAULT_LOCATION" ]]; then
    echo "a vault is already active, aborting..."
    return 1
  fi
}

guard_active_vault () {
  if [[ -z "$CURRENT_VAULT_SRCDIR" ]]; then
    echo "no vault active, aborting..."
    return 1
  fi

  if [[ -z "$CURRENT_VAULT_LOOP" ]]; then
    echo "no vault active, aborting..."
    return 1
  fi

  if [[ -z "$CURRENT_VAULT_DM" ]]; then
    echo "no vault active, aborting..."
    return 1
  fi

  if [[ -z "$CURRENT_VAULT_DIR" ]]; then
    echo "no vault active, aborting..."
    return 1
  fi

  if [[ -z "$CURRENT_VAULT_FILE" ]]; then
    echo "no vault active, aborting..."
    return 1
  fi

  if [[ -z "$CURRENT_VAULT_LOCATION" ]]; then
    echo "no vault active, aborting..."
    return 1
  fi
}

guard_local_vaultopen () {
  local vaultfile=$1
  local localname=$(extract_vault_localname $vaultfile)
  local localhost=$(hostname)

  if [[ "$localname" != "$localhost" ]]; then
    echo "remote"
  else
    echo "local"
  fi
}

extract_vault_localname () {
  local vaultfile=$1
  local extractre="s/\.vault$//"
  local localname=$(echo "$vaultfile" | sed "$extractre")

  echo $localname
}
