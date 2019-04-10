#!/bin/bash

set -euo pipefail

cd /workdir/e2e

instances_to_test=( "a2-iamv2-local-fresh-install-${CHANNEL}.cd.chef.co" "a2-iamv2-local-inplace-upgrade-${CHANNEL}.cd.chef.co" )

for instance in ${instances_to_test[*]}
do
  echo "--- Executing Cypress tests against $instance"
  export CYPRESS_BASE_URL="https://$instance"
  export CYPRESS_RECORD_KEY="$CYPRESS_RECORD_KEY"

  cypress run --record
done
