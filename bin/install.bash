#!/usr/bin/env -S bash -euo pipefail
logfile=$(mktemp)
echo "### Temporary log file '${logfile}'"
# Redirect tee stdout and stderr to logfile
exec 1> >(tee ${logfile}) 2>&1

### EVERYTHING BELOW WILL GO TO CONSOLE AND LOGFILE ############################
set -euo pipefail

function log { printf "%s === %s\n" "$(date -uIns)" "${1}"; }
function err { printf "%s !!! %s\n" "$(date -uIns)" "${1}" >&2; }
function die { err "${1}"; exit 1; }
function indent { sed -E 's/\r$//g;s/\r/\n/g' | sed -E "s/^/    /g"; }
function debug {
  if [[ -v DEBUG ]]; then
    indent
  else
    cat >/dev/null
  fi
}
function ask_no_echo() {
  read -sp "${1} " "${2}"
  # prevents bunching in the log (because input is not logged)
  echo
}
function is_mounted {
  mount | grep " ${1} "
}
function mount_partition {
  if ! (is_mounted "${2}" || mount "${1}" "${2}") |& debug; then
    die "Failed to mount: ${1}"
  fi
  log "Mounted: ${2}"
}
function verify_partition {
  [[ -b "/dev/disk/by-partlabel/${1}" ]] ||
    die "Partition missing: ${1}"
  log "Partition exists: ${1}"
}
function verify_luks_device {
  if ! cryptsetup isLuks "${luks_device}" |& debug; then
    die "Invalid LUKS device: ${luks_device}"
  fi
  log "Valid LUKS device: ${luks_device}"
}
function wait_for() {
  if [[ ! -e "${1}" ]] &>/dev/null; then
    log "Waiting for: ${1}"
    while [[ ! -e "${1}" ]] &>/dev/null; do
      sleep 1
    done
  fi
  log "Found: ${1}"
}
function open_luks_device {
  verify_partition "${luks_name}"
  verify_luks_device
  if [[ ! -b "${lvm_device}" ]]; then
    log "Opening LUKS device: ${luks_device}"
    ask_no_echo "Enter passphrase for ${luks_device}:" PASS
    while ! echo "${PASS}" |
        cryptsetup open "${luks_device}" "${lvm_name}" |& debug; do
      err "Open LUKS failed!"
      ask_no_echo "Enter passphrase for ${luks_device}:" PASS
    done
    wait_for "${lvm_device}"
  fi
  log "LUKS device opened: ${luks_device}"
}
function verify_physical_volume {
  if ! (pvs | grep "${lvm_device}") |& debug; then
    die "Physical volume missing: ${lvm_device}"
  fi
  log "Physical volume exists: ${lvm_device}"
}
function verify_volume_group {
  if ! (vgs | grep "${vg_name}") |& debug; then
    die "Volume group missing: ${vg_name}"
  fi
  log "Volume group exists: ${vg_name}"
}
function verify_logical_volume {
  if ! (lvs -S "vg_name=${vg_name} && lv_name=${1}" | grep "${1}") |& debug; then
    die "Logical volume missing: ${1}"
  fi
  log "Logical volume exists: ${1}"
}
function file {
  nix-shell -p file --run "file -sL ${1}"
}
function verify_root_device {
  if ! (file "${root_device}" | grep "ext4 filesystem") |& debug; then
    die "Invalid root partition: ${root_device}"
  fi
  log "Valid root partition: ${root_device}"
}
function verify_mnt {
  if ! is_mounted "/mnt" |& debug; then
    log "Mounting: /mnt"
    verify_logical_volume "root"
    verify_root_device
    mount_partition "${root_device}" "/mnt"
  fi
  log "Verified: /mnt"
}
function verify_boot_device {
  verify_partition "${boot_name}"
  if ! (file "${boot_device}" | grep "FAT (32 bit)") |& debug; then
    die "Invalid boot device: ${boot_device}"
  fi
  log "Valid boot device: ${boot_device}"
}
function verify_boot {
  if ! is_mounted "/mnt/boot" |& debug; then
    log "Mounting: /mnt/boot"
    verify_boot_device
    mkdir -p "/mnt/boot" |& debug
    mount_partition "${boot_device}" "/mnt/boot"
  fi
  log "Verified: /mnt/boot"
}
function verify_swap_device {
  if ! swaplabel "${swap_device}" |& debug; then
    die "Invalid swap device: ${swap_device}"
  fi
  log "Valid swap device: ${swap_device}"
}
function enable_swap {
  verify_logical_volume "swap"
  verify_swap_device
  if ! (swapon | grep "$(realpath ${swap_device})") |& debug; then
    log "Enabling swap: ${swap_device}"
    swapon "${swap_device}" |& debug || die "Failed to enable swap: ${swap_device}"
  fi
  log "Swap enabled: ${swap_device}"
}
function verify_ssh_keys {
  # adapted from sshd pre-start script
  mkdir -m 0755 -p "${ssh_dir}" |& debug
  if ! [ -s "${rsa_key_path}" ]; then
    if ! [ -h "${rsa_key_path}" ]; then
      rm -f "${rsa_key_path}" |& debug
    fi
    log "Creating SSH host RSA keys"
    ssh-keygen -t "rsa" -b 4096 -f "${rsa_key_path}" -N "" -C "root@${hostname}" |& debug ||
      die "Failed to generate host RSA keys"
  fi
  if ! [ -s "${ed25519_key_path}" ]; then
    if ! [ -h "${ed25519_key_path}" ]; then
      rm -f "${ed25519_key_path}" |& debug
    fi
    log "Creating SSH host ed25519 keys"
    ssh-keygen -t "ed25519" -f "${ed25519_key_path}" -N "" -C "root@${hostname}" |& debug ||
      die "Failed to generate host ed25519 keys"
  fi
  log "Verified SSH host keys"
}
function pause_for_input {
  read -n1 -sp "Press any key to continue..."
  echo
}
function print_key_and_config {
  log "${ed25519_key_path}.pub >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
  cat "${ed25519_key_path}.pub"
  log "hardware-configuration.nix >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>"
  nixos-generate-config --show-hardware-config --no-filesystems
  log "Add these for ${hostname}, rekey secrets, and commit"
  pause_for_input
}

log "Setup started"
[[ -v 1 ]] || die "Expected hostname as first argument"
hostname="${1}"
log "Using hostname: ${hostname}"
repo="${2:-github:thoughtfull-systems/nixfiles}"
log "Using repo: ${repo}"
pause_for_input

boot_name="${hostname}-boot"
boot_device="/dev/disk/by-partlabel/${boot_name}"
luks_name="${hostname}-luks"
luks_device="/dev/disk/by-partlabel/${luks_name}"
lvm_name="${hostname}-lvm"
lvm_device="/dev/mapper/${lvm_name}"
vg_name="${hostname}"
root_device="/dev/mapper/${hostname}-root"
swap_device="/dev/mapper/${hostname}-swap"
open_luks_device
verify_physical_volume
verify_volume_group
verify_mnt
verify_boot
enable_swap
ssh_dir="/mnt/etc/ssh"
rsa_key_path="${ssh_dir}/ssh_host_rsa_key"
ed25519_key_path="${ssh_dir}/ssh_host_ed25519_key"
verify_ssh_keys
print_key_and_config

log "Installation started"
nixos-install --no-root-password --flake "${repo}#${hostname}" |& indent ||
  die "Failed to install NixOS"

log "Installation complete"
cat "${logfile}" >> "/mnt/etc/nixos/install.log"
rm "${logfile}"
