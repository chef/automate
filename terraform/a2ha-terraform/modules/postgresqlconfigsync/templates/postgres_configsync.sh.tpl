#!/bin/bash

set -Eeuo pipefail

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist
LOCKFILE=".postgres.deployment.done"
if [ ! -f ${tmp_path}/$LOCKFILE ] ; then
    echo "Restarting the Postgres cluster to make the config sync for HA proxy" 
    sudo systemctl stop hab-sup
    sudo systemctl start hab-sup
    sudo touch ${tmp_path}/$LOCKFILE
else
    echo "Postgres cluster restart not required"  
fi  

 