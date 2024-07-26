package remotescripts

const POST_CERT_ROTATE_PG = `#!/bin/bash

set -Eeuo pipefail

HAB_NONINTERACTIVE=true
HAB_NOCOLORING=true
HAB_LICENSE=accept-no-persist

PG_ORIGIN_NAME=$(echo "%[1]s" | awk -F/ '{print $1}')

PG_PKG_NAME=$(echo "%[1]s" | awk -F/ '{print $2}')

PGLEADERCHK_ORIGIN_NAME=$(echo "%[2]s" | awk -F/ '{print $1}')
PGLEADERCHK_PKG_NAME=$(echo "%[2]s" | awk -F/ '{print $2}')

PROXY_ORIGIN_NAME=$(echo "%[3]s" | awk -F/ '{print $1}')
PROXY_PKG_NAME=$(echo "%[3]s" | awk -F/ '{print $2}')
LOGCMD='>>/hab/var/automate-ha/svc-load.log 2>&1'

hab svc unload "$PG_ORIGIN_NAME/$PG_PKG_NAME"
hab svc unload "$PGLEADERCHK_ORIGIN_NAME/$PGLEADERCHK_PKG_NAME"
hab svc unload "$PROXY_ORIGIN_NAME/$PROXY_PKG_NAME"

sleep 10

bash -c 'eval hab svc load %[1]s --topology leader --strategy none "$LOGCMD"'
bash -c 'eval hab svc load %[2]s --topology standalone --strategy none --bind database:"$PG_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'
bash -c 'eval hab svc load %[3]s --topology standalone --strategy none --bind database:"$PG_PKG_NAME".default --bind pgleaderchk:"$PGLEADERCHK_PKG_NAME".default --binding-mode=relaxed "$LOGCMD"'

`
