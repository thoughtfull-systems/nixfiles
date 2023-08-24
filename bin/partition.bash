#!/usr/bin/env -S bash -euo pipefail
logfile=$(mktemp)
echo "### Temporary log file '${logfile}'"
# Redirect tee stdout and stderr to logfile
exec 1> >(tee ${logfile}) 2>&1

### EVERYTHING BELOW WILL GO TO CONSOLE AND LOGFILE ############################
set -euo pipefail

function log { printf "%s === %s\n" "$(date -uIns)" "${1}"; }
function die { printf "%s !!! %s\n" "$(date -uIns)" "${1}" >&2; exit 1; }
function indent { sed -E 's/\r$//g;s/\r/\n/g' | sed -E "s/^/    /g"; }

log "Partitioning starting $(date)"

function ask() {
  msg="??? ${1} "
  if [[ -v 2 ]]; then
    read -p "${msg}" ${2}
  else
    read -p "${msg}"
  fi
  # prevents bunching in the log (because input is not logged)
  echo
}

function confirm {
  ask "${1} (y/N)"
  [[ ${REPLY} =~ ^[Yy].* ]]
}

# Validate hostname argument
[[ -v 1 ]] || die "Expected hostname as first argument"
hostname="${1}"


scriptdir="$(dirname $(realpath ${0}))"
repo="${3:-github:thoughtfull-systems/nixfiles}"

### VARIABLES ##################################################################
# devices
boot_name="${hostname}-boot"
boot_device="/dev/disk/by-partlabel/${boot_name}"
luks_name="${hostname}-luks"
luks_device="/dev/disk/by-partlabel/${luks_name}"
lvm_name="${hostname}-lvm"
lvm_device="/dev/mapper/${lvm_name}"
vg_name="${hostname}"
swap_device="/dev/mapper/${vg_name}-swap"
root_device="/dev/mapper/${vg_name}-root"

### FUNCTIONS ##################################################################
# General
function ask_no_echo() {
  msg="??? ${1} "
  read -sp "${msg}" ${2}
  # prevents bunching in the log (because input is not logged)
  echo
}

function really_sure {
  echo "??? Are your REALLY sure you want to ${1}? (ALL DATA WILL BE LOST)"
  ask "(Please enter YES in all caps):"

  [[ $REPLY = "YES" ]]
}

# Partitions & devices
function has_device {
  [[ -b "${1}" ]] &>/dev/null
}

function is_mounted {
  (mount | grep " ${1} ") &>/dev/null
}

function ensure_unmounted {
  if is_mounted "${1}"; then
    log "Unmounting '${1}'"
    umount "${1}" |& indent
  fi
}

function ensure_mounted {
  if ! is_mounted "${1}" && ! is_mounted "\$(realpath ${1})"; then
    log "Mounting '${1}' to '${2}'"
    (mount "${1}" "${2}" 2>/dev/null ||
       mount "$(realpath ${1})" "${2}") |& indent ||
      die "Failed to mount '${1}'"
  fi
}

# LUKS
function is_luks { cryptsetup isLuks "${luks_device}"; }

function wait_for() {
  if [[ ! -e "${1}" ]] &>/dev/null; then
    log "Waiting for '${1}'..."
    while [[ ! -e "${1}" ]] &>/dev/null; do
      sleep 1
    done
  fi
}

function ensure_luks_closed {
  if has_device "${lvm_device}"; then
    log "Closing LUKS device '${lvm_device}'"
    cryptsetup close "${lvm_device}"
  fi
}

function open_luks {
  log "Using LUKS device '${luks_device}'"
  log "Opening LUKS device '${luks_device}' as '${lvm_name}'"
  echo "${1}" |
    cryptsetup open "${luks_device}" "${lvm_name}" |& indent ||
    die "Failed to open '${luks_device}'"
  wait_for "/dev/mapper/${lvm_name}"
}

# LVM
function has_pv {
  (pvs | grep "${lvm_device}") &>/dev/null
}

function ensure_pv_removed {
  if has_pv; then
    log "Removing physical volume '${lvm_device}'"
    pvremove -y "${lvm_device}" |& indent
  fi
}

function has_vg {
  (vgs | grep "${vg_name}") &>/dev/null
}

function ensure_vg_removed {
  if has_vg; then
    log "Removing volume group '${vg_name}'"
    vgremove -y "${vg_name}" |& indent
  fi
}

function has_lv {
  (lvs -S "vg_name=${vg_name} && lv_name=${1}" |
    grep "${1}") &>/dev/null
}

function ensure_lv_removed {
  if has_lv "${1}"; then
    log "Removing volume '${1}'"
    lvremove -y "${vg_name}/${1}" |& indent
  fi
}

# FAT32
function is_boot_fat32 {
  # https://wiki.archlinux.org/title/FAT#Detecting_FAT_type recommends either
  # file or minfo
  (nix-shell -p file --run "file -sL ${boot_device}" | grep "FAT (32 bit)") &>/dev/null
}

# Swap
function is_swap {
  swaplabel "${swap_device}" &>/dev/null
}

function is_swapon {
  (swapon | grep "$(realpath ${swap_device})") &>/dev/null
}

function ensure_swapoff {
  if is_swapon; then
    swapoff "$(realpath ${swap_device})" |& indent ||
      die "Failed to disable swap '${swap_device}'"
  fi
}

function is_root_ext4 {
  (nix-shell -p file --run "file -sL ${root_device}" | grep "ext4 filesystem") &>/dev/null
}

function was_partitioned {
  [[ ${partitioned} -eq 0 ]]
}

### PARTITION TABLE ###
partitioned=1
log "Current partition table..."
parted -l |& indent

# Create new partition table?
log "Partition table must include ${boot_name} (FAT32) and ${luks_name} (LUKS)"
if confirm "Create new partition table (ALL DATA WILL BE LOST)?"; then
  ask "Partition which disk?" disk
  while ! parted -s "${disk}" print &>/dev/null; do
    ask "'${disk}' does not exist; partition which disk?" disk
  done

  if really_sure "erase and partition '${disk}'"; then
    partitioned=0
    ensure_unmounted "/mnt/boot"
    ensure_unmounted "/mnt"
    ensure_lv_removed "root"
    ensure_swapoff
    ensure_lv_removed "swap"
    ensure_vg_removed
    ensure_pv_removed
    ensure_luks_closed
    log "Creating partition table"
    parted -fs "${disk}" mklabel gpt |& indent || die "Failed to create partition table"
    log "Creating boot partition (1G)"
    parted -fs "${disk}" mkpart "${boot_name}" fat32 1MiB 1GiB |& indent ||
      die "Failed to create boot partition"
    parted -fs "${disk}" set 1 esp |& indent || die "Failed to mark boot partition as ESP"
    log "Creating LUKS partition with free space"
    parted -fs "${disk}" mkpart "${luks_name}" 1GiB 100% |& indent ||
      die "Failed to create LUKS partition"
  fi
  wait_for "${boot_device}"
  wait_for "${luks_device}"
else
  log "Using existing partition table"
fi

# Verify partitions
has_device "${boot_device}" || die "Missing boot partition '${boot_name}'"
has_device "${luks_device}" || die "Missing LUKS partition '${luks_name}'"

### BOOT DEVICE ###
if was_partitioned ||
    (! is_boot_fat32 &&
       confirm "Format as FAT32 '${boot_device}'?" &&
       really_sure "format as FAT32 '${boot_device}'")
then
  ensure_unmounted "/mnt/boot"
  log "Formatting as FAT32 '${boot_device}'"
  mkfs.fat -F 32 -n BOOT "${boot_device}" |& indent ||
    die "Failed to format as FAT32 '${boot_device}'"
fi

if is_boot_fat32; then
  log "Using boot device '${boot_device}'"
else
  die "Unsuitable boot device '${boot_device}'"
fi

### LUKS DEVICE ###
if was_partitioned ||
    (! is_luks &&
       confirm "Format as LUKS '${luks_device}'?" &&
       really_sure "format as LUKS '${luks_device}'")
then
  ensure_unmounted "/mnt/boot"
  ensure_unmounted "/mnt"
  ensure_lv_removed "root"
  ensure_swapoff
  ensure_lv_removed "swap"
  ensure_vg_removed
  ensure_pv_removed
  ensure_luks_closed
  log "Formatting as LUKS '${luks_device}'"
  (ask_no_echo "Please enter your passphrase:" PASS
   ask_no_echo "Please confirm your passphrase:" CONFIRM
   while [[ "${PASS}" != "${CONFIRM}" ]]; do
     log "Password and confirmation do not match..."
     ask_no_echo "Please enter your passphrase:" PASS
     ask_no_echo "Please confirm your passphrase:" CONFIRM
   done
   echo "${PASS}" | cryptsetup luksFormat "${luks_device}" |& \
     indent
   open_luks "${PASS}") || die "Failed to format as LUKS '${luks_device}'"
fi

if is_luks; then
  if ! has_device "${lvm_device}" ; then
    ask_no_echo "Please enter your passphrase:" PASS
    while ! open_luks "${PASS}"; do
      log "Incorrect password..."
      ask_no_echo "Please enter your passphrase:" PASS
    done
  fi
else
  die "Unsuitable LUKS device '${luks_device}'"
fi

### LVM ###
# Check LVM physical volume
if ! has_pv; then
  log "Creating '${lvm_device}' LVM physical volume"
  pvcreate "${lvm_device}" |& indent ||
    die "Failed to create '${lvm_device}' LVM physical volume"
else
  log "Using '${lvm_device}' LVM physical volume"
fi

# Check LVM volume group
if ! has_vg; then
  log "Creating '${vg_name}' LVM volume group"
  (vgcreate "${vg_name}" "${lvm_device}" |& indent) ||
    die "Failed to create '${vg_name}' LVM volume group"
else
  log "Using '${vg_name}' LVM volume group"
fi

## SWAP ##
# Create swap LVM volume
if was_partitioned || ! has_lv "swap"; then
  log "Creating 'swap' LVM volume"
  ensure_swapoff
  ensure_lv_removed "swap"
  (if confirm "Should 'swap' be large enough for hibertation?"; then
     swap_factor=3
   else
     swap_factor=2
   fi
   mem_total=$(($(grep MemTotal /proc/meminfo | grep -o [[:digit:]]\*) \
                  / 1000000))
   swap_size=$((${mem_total}*${swap_factor}))
   log "Creating '${vg_name}-swap' with ${swap_size}G"
   lvcreate --size "${swap_size}G" --name swap "${vg_name}" |& indent
   wait_for "/dev/mapper/${vg_name}-swap")||
    die "Failed to create 'swap' LVM volume"
fi

# Format swap volume
if was_partitioned ||
    (! is_swap &&
       confirm "Format as swap '${swap_device}'?" &&
       really_sure "format as swap '${swap_device}'")
then
  log "Formatting as swap '${swap_device}'"
  ensure_swapoff
  mkswap -L "swap" "${swap_device}" |& indent ||
    die "Failed to format as swap '${swap_device}'"
fi

if has_lv "swap" &&
    is_swap; then
  if ! is_swapon; then
    log "Enabling swap '${swap_device}'"
    swapon "${swap_device}" |& indent ||
      die "Failed to enable swap '${swap_device}'"
  fi
  log "Using 'swap' LVM volume"
else
  die "Unsuitable swap volume 'swap'"
fi

## ROOT ##
# Check root logical volume filesystem
if was_partitioned || ! has_lv "root"; then
  ensure_unmounted "/mnt"
  ensure_lv_removed "root"
  log "Creating 'root' LVM volume"
  (lvcreate --extents 100%FREE --name root ${vg_name} |& indent
   # https://man.archlinux.org/man/e2scrub.8 says e2scrub needs at least 256MiB
   # to create a snapshot in order to check the metadata of an LVM ext
   # filesystem
   log "Leaving free space for e2scrub"
   lvreduce -y --size -300M ${vg_name}/root |& indent
   wait_for "/dev/mapper/${vg_name}-root") ||
    die "Failed to create 'root' LVM volume"
fi

# format root
if was_partitioned ||
    (! is_root_ext4 &&
       confirm "Format as ext4 '${root_device}'" &&
       really_sure "format as ext4 '${root_device}'")
then
  log "Using root LVM volume 'root'"
  ensure_unmounted "/mnt"
  log "Formatting as ext4 '${root_device}'"
  mkfs.ext4 -L "root" "${root_device}" |& indent ||
    die "Failed to format as ext4 '${root_device}'"
fi

if is_root_ext4; then
  log "Using root device '${root_device}'"
  # Mount root
  ensure_mounted "${root_device}" /mnt
else
  die "Unsuitable root device '${root_device}'"
fi

# Mount boot
mkdir -p /mnt/boot |& indent
ensure_mounted "${boot_device}" /mnt/boot

## COPY LOG ##
mkdir -p /mnt/etc/nixos
log "Partitioning complete"
cat "${logfile}" >> "/mnt/etc/nixos/install.log"
rm "${logfile}"
