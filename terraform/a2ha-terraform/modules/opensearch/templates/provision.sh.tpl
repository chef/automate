#!/bin/bash

set -Eeuo pipefail

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

# 
echo "${backup_config_s3}"

SERVICE_UP_TIME=$(echo yes | hab svc status  | awk  '{print $5}' | tail -1)

# Following loop will hold adding credentials in keystore until
# services are up and ran for 30 seconds and gives enough
# time to remove temp keystore file 'opensearch.keystore'

# If you get following error, ===> `[: : integer expression expected`
# try `until [[ "$SERVICE_UP_TIME" -gt 30 ]]` instead of `until [ "$SERVICE_UP_TIME" -gt 30 ]`
if [ "${backup_config_s3}" == "true" ]; then

max=15
n=0
until [ "$SERVICE_UP_TIME" -gt 30 -a $n -lt $max ]
do
  sleep 5
  SERVICE_UP_TIME=$(echo yes | hab svc status  | awk  '{print $5}' | tail -1)
  echo "No services are loaded"
  echo "Sleeping for 5 seconds"
  n=$((n+1))
done

# Adding aws access and secret keys once all services are up

  echo "Setting up keystore"
  echo yes | hab svc status
  export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"
  echo $OPENSEARCH_PATH_CONF
  echo "${access_key}" | hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore add --stdin --force s3.client.default.access_key
  echo "${secret_key}" | hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore add --stdin --force s3.client.default.secret_key
  hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore list
  sudo chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore
  hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore list
  curl -k -X POST "https://127.0.0.1:${listen_port}/_nodes/reload_secure_settings?pretty" -u $opensearch_username:$opensearch_user_password
fi