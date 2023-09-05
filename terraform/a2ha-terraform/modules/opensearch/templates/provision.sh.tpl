#!/bin/bash

set -Eeuo pipefail

# Function to check SELinux status and mode
check_selinux() {
    # Check if /etc/selinux exists (common to RHEL, CentOS, Fedora)
    if [ -e /etc/selinux/config ]; then
        echo "SELinux configuration file found."

        # Check for SELinux status and mode
        selinux_status=$(getenforce)
        selinux_mode=$(awk -F= '/^SELINUX=/ {print $2}' /etc/selinux/config)

        echo "SELinux Status: $selinux_status"
        echo "SELinux Mode: $selinux_mode"

        # If SELinux is enabled (Enforcing), set it to Permissive
        if [ "$selinux_status" == "Enforcing" ]; then
            echo "SELinux is currently in Enforcing mode. Changing to Permissive..."
            setenforce Permissive
            echo "SELinux mode set to Permissive."
        fi

    # Check if /etc/selinux does not exist (common to Debian, Ubuntu)
    elif [ -e /etc/default/grub ]; then
        echo "SELinux configuration file not found."

        # Check if "selinux=1" is present in grub (Enforcing)
        if grep -q "selinux=1" /etc/default/grub; then
            echo "SELinux is enabled (Enforcing) in GRUB."

            # Change GRUB to Permissive
            sed -i 's/selinux=1/selinux=0/' /etc/default/grub
            # update-grub
            # echo "GRUB configuration updated to Permissive."
        # fi

        # SELinux not found in grub (Disabled or Permissive)
        else
            echo "SELinux is not found or is already disabled in GRUB."
        fi

    # SELinux configuration file not found (SUSE, Amazon Linux, etc.)
    else
        echo "SELinux configuration file not found."
    fi
}

# Check SELinux
check_selinux

umask 0022

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist

OS_ORIGIN_NAME=$(echo "${opensearch_pkg_ident}" | awk -F/ '{print $1}')
export OS_ORIGIN_NAME
OPENSEARCHSIDECAR_ORIGIN_NAME=$(echo "${opensearchsidecar_pkg_ident}" | awk -F/ '{print $1}')
export OPENSEARCHSIDECAR_ORIGIN_NAME
OS_PKG_NAME=$(echo "${opensearch_pkg_ident}" | awk -F/ '{print $2}')
export OS_PKG_NAME
OPENSEARCHSIDECAR_PKG_NAME=$(echo "${opensearchsidecar_pkg_ident}" | awk -F/ '{print $2}')
export OPENSEARCHSIDECAR_PKG_NAME

mkdir -p /hab/user/"$OS_PKG_NAME"/config
cp -f ${tmp_path}/opensearch-user.toml /hab/user/"$OS_PKG_NAME"/config/user.toml

mkdir -p /hab/user/"$OPENSEARCHSIDECAR_PKG_NAME"/config
cp -f ${tmp_path}/opensearchsidecar.toml /hab/user/"$OPENSEARCHSIDECAR_PKG_NAME"/config/user.toml

# Creating mount path for elasticsearch backup 
sudo mkdir -p ${nfs_mount_path}/opensearch
sudo chown hab:hab ${nfs_mount_path}/opensearch/

wait_for_aib_extraction() {
  max=20
  n=0
  until [ $n -ge $max ]; do
    ls ${backend_aib_dest_file}.DONE >/dev/null 2>&1  && break
    n=$((n+1))
    echo "Waiting for habitat package installations from ${backend_aib_dest_file} to complete"
    sleep 30
  done
  if [[ $n -ge $max ]]; then
    echo "Failed waiting for ${backend_aib_dest_file} to be transferred within $max iterations!"
    exit 1
  fi
}

while [ ! -f /hab/sup/default/LOCK ]; do
  sleep 30
  echo 'waiting for habitat supervisor lock to show up'
done

wait_for_aib_extraction

# TODO: remove this workaround once the following issue is resolved
# https://github.com/habitat-sh/habitat/issues/6260
export LOGCMD='>>${tmp_path}/svc-load.log 2>&1'

if [ -e /hab/sup/default/specs/"$OS_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${opensearch_pkg_ident}\"" /hab/sup/default/specs/"$OS_PKG_NAME".spec; then
     # unload the old pkg_ident and then load in the new
     #echo "Upgrading the backend package"
     #source /hab/sup/default/SystemdEnvironmentFile.sh
     #appliedConfigFile="config.$RANDOM.toml"
     #automate-backend-ctl applied --svc=automate-ha-opensearch | tail -n +2 > $appliedConfigFile

     bash -c 'sysctl -w vm.max_map_count=262144'
     hab svc unload "$OS_ORIGIN_NAME/$OS_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${opensearch_pkg_ident} ${opensearch_svc_load_args} "$LOGCMD"'
     # 1. need to remove the backend_config
     # 2. apply only when it has size > 0 byte
     #if [  ! -s  $appliedConfigFile ]
     # then
     #   echo "$FILE has zero size, no need to apply the config"
     # else
	   #   echo "external config is present lets put some sleep and apply the config"
     #   sleep 10
     #   bash -c 'hab config apply automate-ha-opensearch.default $(date '+%s') $appliedConfigFile'
     #   echo "Config Applied"
     #   #rm $appliedConfigFile
     #fi
  fi
else
  bash -c 'sysctl -w vm.max_map_count=262144'
  bash -c 'eval hab svc load ${opensearch_pkg_ident} ${opensearch_svc_load_args} "$LOGCMD"'
fi

if [ -e /hab/sup/default/specs/"$OPENSEARCHSIDECAR_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${opensearchsidecar_pkg_ident}\"" /hab/sup/default/specs/"$OPENSEARCHSIDECAR_PKG_NAME".spec; then
     # stop and unload the old pkg_ident and then load in the new
     hab svc unload "$OPENSEARCHSIDECAR_ORIGIN_NAME/$OPENSEARCHSIDECAR_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${opensearchsidecar_pkg_ident} ${opensearchsidecar_svc_load_args} --bind opensearch:"$OS_PKG_NAME".default --binding-mode=strict "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${opensearchsidecar_pkg_ident} ${opensearchsidecar_svc_load_args} --bind opensearch:"$OS_PKG_NAME".default --binding-mode=strict "$LOGCMD"'
fi

