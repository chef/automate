#!/bin/bash

set -Eeuo pipefail

umask 0022

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist

PG_ORIGIN_NAME=$(echo "${postgresql_pkg_ident}" | awk -F/ '{print $1}')
export PG_ORIGIN_NAME
PG_PKG_NAME=$(echo "${postgresql_pkg_ident}" | awk -F/ '{print $2}')
export PG_PKG_NAME

PGLEADERCHK_ORIGIN_NAME=$(echo "${pgleaderchk_pkg_ident}" | awk -F/ '{print $1}')
export PGLEADERCHK_ORIGIN_NAME
PGLEADERCHK_PKG_NAME=$(echo "${pgleaderchk_pkg_ident}" | awk -F/ '{print $2}')
export PGLEADERCHK_PKG_NAME

PROXY_ORIGIN_NAME=$(echo "${proxy_pkg_ident}" | awk -F/ '{print $1}')
export PROXY_ORIGIN_NAME
PROXY_PKG_NAME=$(echo "${proxy_pkg_ident}" | awk -F/ '{print $2}')
export PROXY_PKG_NAME

mkdir -p /hab/user/{$PG_PKG_NAME,$PGLEADERCHK_PKG_NAME,$PROXY_PKG_NAME}/config
cp -f ${tmp_path}/postgresql-user.toml /hab/user/"$PG_PKG_NAME"/config/user.toml
cp -f ${tmp_path}/pgleaderchk-user.toml /hab/user/"$PGLEADERCHK_PKG_NAME"/config/user.toml
cp -f ${tmp_path}/proxy-user.toml /hab/user/"$PROXY_PKG_NAME"/config/user.toml

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

if [ -e /hab/sup/default/specs/"$PG_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${postgresql_pkg_ident}\"" /hab/sup/default/specs/"$PG_PKG_NAME".spec; then
     # unload the old pkg_ident and then load in the new
     hab svc unload "$PG_ORIGIN_NAME/$PG_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${postgresql_pkg_ident} ${postgresql_svc_load_args} "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${postgresql_pkg_ident} ${postgresql_svc_load_args} "$LOGCMD"'
fi

if [ -e /hab/sup/default/specs/"$PGLEADERCHK_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${pgleaderchk_pkg_ident}\"" /hab/sup/default/specs/"$PGLEADERCHK_PKG_NAME".spec; then
     # stop and unload the old pkg_ident and then load in the new
     hab svc unload "$PGLEADERCHK_ORIGIN_NAME/$PGLEADERCHK_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${pgleaderchk_pkg_ident} ${pgleaderchk_svc_load_args} --bind database:"$PG_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${pgleaderchk_pkg_ident} ${pgleaderchk_svc_load_args} --bind database:"$PG_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'
fi

if [ -e /hab/sup/default/specs/"$PROXY_PKG_NAME".spec ]; then
  if ! grep -q "ident *= *\"${proxy_pkg_ident}\"" /hab/sup/default/specs/"$PROXY_PKG_NAME".spec; then
     # stop and unload the old pkg_ident and then load in the new
     hab svc unload "$PROXY_ORIGIN_NAME/$PROXY_PKG_NAME"
     sleep 10
     bash -c 'eval hab svc load ${proxy_pkg_ident} ${proxy_svc_load_args} --bind database:"$PG_PKG_NAME".default --bind pgleaderchk:"$PGLEADERCHK_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'
  fi
else
  bash -c 'eval hab svc load ${proxy_pkg_ident} ${proxy_svc_load_args} --bind database:"$PG_PKG_NAME".default --bind pgleaderchk:"$PGLEADERCHK_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'
fi
