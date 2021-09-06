#!/bin/bash

set -Eeuo pipefail

umask 0022

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist

METRICBEAT_ORIGIN_NAME=$(echo "${metricbeat_pkg_ident}" | awk -F/ '{print $1}')
export METRICBEAT_ORIGIN_NAME
METRICBEAT_PKG_NAME=$(echo "${metricbeat_pkg_ident}" | awk -F/ '{print $2}')
export METRICBEAT_PKG_NAME

mkdir -p /hab/user/"$METRICBEAT_PKG_NAME"/config
cp -f ${tmp_path}/metricbeat_user.toml /hab/user/"$METRICBEAT_PKG_NAME"/config/user.toml

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

if [ -e /hab/sup/default/specs/"$METRICBEAT_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${metricbeat_pkg_ident}\"" /hab/sup/default/specs/"$METRICBEAT_PKG_NAME".spec; then
     # unload the old pkg_ident and then load in the new
     hab svc unload "$METRICBEAT_ORIGIN_NAME/$METRICBEAT_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${metricbeat_pkg_ident} ${metricbeat_svc_load_args} ${metricbeat_svc_binds} --binding-mode=strict "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${metricbeat_pkg_ident} ${metricbeat_svc_load_args} ${metricbeat_svc_binds} --binding-mode=strict "$LOGCMD"'
fi
