vaultsync () {
  local remote=$1
  if [[ -z "$remote" ]]; then
    echo "no remote name specified, aborting..."
    return 1
  fi
  guard_is_valid_remote "$remote" || { return 1; }

  local localname=$(cat .vault/local)
  local localvaultfile="$localname.vault"
  echo "opening $localvaultfile..."
  vaultopen $localvaultfile || { return 1; }
  store_vault_as_synclocal
  echo "synclocal descriptor stored"

  echo "returning to srcdir..."
  cd "$CURRENT_VAULT_SRCDIR"
  unset_active_vault
  local remotevaultfile="$remote.vault"
  echo "opening $remotevaultfile..."
  vaultopen $remotevaultfile || {
    failed_sync_close_synclocal
    unset_descriptor_vars
    return 1;
  }
  store_vault_as_syncremote
  echo "syncremote descriptor stored"

  unset_active_vault
  restore_vault_from_descriptor "$VAULT_DESCRIPTOR_SYNCLOCAL"
  echo "synclocal descriptor restored, returning to the local vault..."
  cd "$CURRENT_VAULT_DIR"
  git fetch "$remote"
  echo "$remote fetched, closing..."
  vaultclose

  restore_vault_from_descriptor "$VAULT_DESCRIPTOR_SYNCREMOTE"
  echo "syncremote descriptor restored, closing..."
  vaultclose

  unset_descriptor_vars
  echo "done."
}

failed_sync_close_synclocal () {
  unset_active_vault
  restore_vault_from_descriptor "$VAULT_DESCRIPTOR_SYNCLOCAL"
  cd "$CURRENT_VAULT_DIR"
  vaultclose
}

store_vault_as_synclocal () {
  local descriptor=$(make_descriptor_from_current_vault)

  export VAULT_DESCRIPTOR_SYNCLOCAL="$descriptor"
}

store_vault_as_syncremote () {
  local descriptor=$(make_descriptor_from_current_vault)

  export VAULT_DESCRIPTOR_SYNCREMOTE="$descriptor"
}

unset_descriptor_vars () {
  export VAULT_DESCRIPTOR_SYNCLOCAL=""
  export VAULT_DESCRIPTOR_SYNCREMOTE=""
}

make_descriptor_from_current_vault () {
  local descriptor="$CURRENT_VAULT_SRCDIR"
  local descriptor="$descriptor:$CURRENT_VAULT_LOOP"
  local descriptor="$descriptor:$CURRENT_VAULT_DM"
  local descriptor="$descriptor:$CURRENT_VAULT_DIR"
  local descriptor="$descriptor:$CURRENT_VAULT_FILE"
  local descriptor="$descriptor:$CURRENT_VAULT_LOCATION"

  echo $descriptor
}

restore_vault_from_descriptor () {
  local descriptor=$1

  IFS=":" read \
    CURRENT_VAULT_SRCDIR \
    CURRENT_VAULT_LOOP \
    CURRENT_VAULT_DM \
    CURRENT_VAULT_DIR \
    CURRENT_VAULT_FILE \
    CURRENT_VAULT_LOCATION < <(echo "$descriptor")

  export CURRENT_VAULT_SRCDIR
  export CURRENT_VAULT_LOOP
  export CURRENT_VAULT_DM
  export CURRENT_VAULT_DIR
  export CURRENT_VAULT_FILE
  export CURRENT_VAULT_LOCATION
}
