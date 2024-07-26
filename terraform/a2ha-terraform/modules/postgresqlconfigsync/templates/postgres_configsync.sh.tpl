#!/bin/bash

set -Eeuo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist
LOCKFILE=".postgres.deployment.done"

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
export LOGCMD='>>${tmp_path}/svc-load.log 2>&1'

if [ ! -f ${tmp_path}/$LOCKFILE ] ; then
    echo "Restarting the Postgres cluster to make the config sync for HA proxy" 
    # stop and unload the old pkg_ident and then load in the new
    hab svc unload "$PGLEADERCHK_ORIGIN_NAME/$PGLEADERCHK_PKG_NAME"
    hab svc unload "$PROXY_ORIGIN_NAME/$PROXY_PKG_NAME"

    # stop and unload the old pkg_ident and then load in the new
    sleep 5
    bash -c 'eval hab svc load ${proxy_pkg_ident} ${proxy_svc_load_args} --bind database:"$PG_PKG_NAME".default --bind pgleaderchk:"$PGLEADERCHK_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'
    bash -c 'eval hab svc load ${pgleaderchk_pkg_ident} ${pgleaderchk_svc_load_args} --bind database:"$PG_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'

    sudo touch ${tmp_path}/$LOCKFILE
else
    echo "Postgres cluster restart not required"  
fi  

 