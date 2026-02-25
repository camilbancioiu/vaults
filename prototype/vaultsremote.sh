vaultinit () {
  local vaultname=$1

  if [[ -z "$vaultname" ]]; then
    echo "vault name not given, aborting..."
    return 1
  fi

  if [[ -d ".vault" ]]; then
    echo "vault already initialized, aborting..."
    return 1
  fi

  mkdir .vault
  touch .vault/name
  touch .vault/remoteStore
  touch .vault/local
  touch .vault/remotes

  echo "$name" > .vault/name
  echo $(hostname) > .vault/local
}

vaultup () {
  guard_is_vault_srcdir || { return 1; }
  guard_no_active_vault || { return 1; }

  local vaultname=$(cat .vault/name)
  local localname=$(cat .vault/local)
  local remoteStore=$(cat .vault/remoteStore)
  local vaultfile="$localname.vault"
  local uploadfile="$remoteStore/$vaultname/$vaultfile"

  echo $vaultfile
  echo $uploadfile
  rsync -ivz $vaultfile $uploadfile

  local logfile="$localname.log"
  local uploadlogfile="$remoteStore/$vaultname/$logfile"

  echo $logfile
  echo $uploadlogfile
  rsync -ivz $logfile $uploadlogfile
}

vaultdown () {
  guard_is_vault_srcdir || { return 1; }
  guard_no_active_vault || { return 1; }

  local vaultname=$(cat .vault/name)
  local remoteStore=$(cat .vault/remoteStore)
  local remotes=$(cat .vault/remotes)

  for remote in $remotes
  do
      echo
      echo "---- $remote: ------------------"

      local downloadfile="$remoteStore/$vaultname/$remote.vault"
      echo "$downloadfile"
      rsync -ivz $downloadfile "$remote.vault"

      local downloadlogfile="$remoteStore/$vaultname/$remote.log"
      echo "$downloadlogfile"
      rsync -ivz $downloadlogfile "$remote.log"
  done
}

vaultcheck () {
  guard_is_vault_srcdir || { return 1; }
  guard_no_active_vault || { return 1; }

  local remote=$1
  if [[ -n "$remote" ]]; then
    guard_is_valid_remote "$remote" || { return 1; }
    vaultcheck_individual_remote $remote
    return
  fi

  local vaultname=$(cat .vault/name)
  local remoteStore=$(cat .vault/remoteStore)
  local remotes=$(cat .vault/remotes)

  for remote in $remotes
  do
      echo
      echo "---- $remote: ------------------"

      vaultcheck_individual_remote $remote
  done
}

vaultcheck_individual_remote () {
  local localname=$(cat .vault/local)
  local remote=$1

  local local_log="$localname.log"
  local remote_log="$remote.log"

  diff -u "$local_log" "$remote_log"
}

guard_is_valid_remote () {
  local remote=$1

  grep -q -x -F "$remote" ".vault/remotes" || { return 1; }
}
