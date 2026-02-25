createloop () {
  local filepartition=$1
  outloop=$(udisksctl loop-setup -f "$filepartition")
  devloop=$(elementdot "$outloop" 5)
  echo "$devloop"
}

deleteloop() {
  local devloop=$1
  udisksctl loop-delete -b "$devloop"
}

mountlockeddev () {
  local lockeddev=$1
  if [[ -z "$lockeddev" ]]; then
    echo "locked device path required"
    return 1
  fi

  outunlock=$(udisksctl unlock -b "$lockeddev") || { return 1; }

  devunlocked=$(elementdot "$outunlock" 4)
  outmount=$(udisksctl mount -b "$devunlocked" $@) || { return 1; }

  mounteddev=$(element "$outmount" 2)
  mountdir=$(element "$outmount" 4)
  echo "$mounteddev" "$mountdir"
}

element () {
  local string=$1
  local index=$2
  if [[ -z "$string" ]]; then
    echo ""
    return 0
  fi

  echo "$string" | cut -d ' ' -f $index
}

elementdot () {
  local string=$1
  local index=$2
  if [[ -z "$string" ]]; then
    echo ""
    return 0
  fi

  echo "$string" | cut -d ' ' -f $index | cut -d '.' -f 1
}

lockmounteddev () {
  local mounteddev=$1
  local unlockeddev=$2
  if [[ -z "$mounteddev" ]]; then
    echo "mounted device path required"
    return 1
  fi
  if [[ -z "$unlockeddev" ]]; then
    echo "unlocked device path required"
    return 1
  fi

  outunmount=$(udisksctl unmount -b "$mounteddev") || { return 1; }

  outlock=$(udisksctl lock -b "$unlockeddev") || { return 1; }
}
