#!/bin/bash

set -Eeuo pipefail

umask 0022

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist

ES_ORIGIN_NAME=$(echo "${elasticsearch_pkg_ident}" | awk -F/ '{print $1}')
export ES_ORIGIN_NAME
ELASTICSIDECAR_ORIGIN_NAME=$(echo "${elasticsidecar_pkg_ident}" | awk -F/ '{print $1}')
export ELASTICSIDECAR_ORIGIN_NAME
ES_PKG_NAME=$(echo "${elasticsearch_pkg_ident}" | awk -F/ '{print $2}')
export ES_PKG_NAME
ELASTICSIDECAR_PKG_NAME=$(echo "${elasticsidecar_pkg_ident}" | awk -F/ '{print $2}')
export ELASTICSIDECAR_PKG_NAME

mkdir -p /hab/user/"$ES_PKG_NAME"/config
cp -f ${tmp_path}/elasticsearch-user.toml /hab/user/"$ES_PKG_NAME"/config/user.toml

mkdir -p /hab/user/"$ELASTICSIDECAR_PKG_NAME"/config
cp -f ${tmp_path}/elasticsidecar.toml /hab/user/"$ELASTICSIDECAR_PKG_NAME"/config/user.toml

# Creating mount path for elasticsearch backup 
sudo mkdir -p /mnt/automate_backups/elasticsearch
sudo chown hab:hab /mnt/automate_backups/elasticsearch/

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

if [ -e /hab/sup/default/specs/"$ES_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${elasticsearch_pkg_ident}\"" /hab/sup/default/specs/"$ES_PKG_NAME".spec; then
     # unload the old pkg_ident and then load in the new
     hab svc unload "$ES_ORIGIN_NAME/$ES_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${elasticsearch_pkg_ident} ${elasticsearch_svc_load_args} "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${elasticsearch_pkg_ident} ${elasticsearch_svc_load_args} "$LOGCMD"'
fi

if [ -e /hab/sup/default/specs/"$ELASTICSIDECAR_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${elasticsidecar_pkg_ident}\"" /hab/sup/default/specs/"$ELASTICSIDECAR_PKG_NAME".spec; then
     # stop and unload the old pkg_ident and then load in the new
     hab svc unload "$ELASTICSIDECAR_ORIGIN_NAME/$ELASTICSIDECAR_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${elasticsidecar_pkg_ident} ${elasticsidecar_svc_load_args} --bind elasticsearch:"$ES_PKG_NAME".default --binding-mode=strict "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${elasticsidecar_pkg_ident} ${elasticsidecar_svc_load_args} --bind elasticsearch:"$ES_PKG_NAME".default --binding-mode=strict "$LOGCMD"'
fi
