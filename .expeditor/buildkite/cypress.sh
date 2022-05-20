#!/bin/bash

#
# cypress.sh:
# this test file executes cypress automated ui tests against our long-lived
# dev and acceptance deployments of automate on every deploy. it's triggered
# from the .expeditor/deploy.pipeline.yml configuration in this repository
#

set -euo pipefail

cd /workdir/e2e

data=$(curl --silent "https://a2-${CHANNEL}.cd.chef.co/assets/data.json")
instances_to_test=$(jq -nr --argjson data "$data" '$data[] | select(.tags | any(. == "e2e")) | .fqdn')

for instance in ${instances_to_test[*]}
do
  if ! jq -enr --arg fqdn "$instance" --argjson data "$data" '$data[] | select(.fqdn == $fqdn) | .tags | any(. == "saml")'; then
    export CYPRESS_SKIP_SSO=true
  fi

  echo "--- Executing Cypress tests against $instance"
  export CYPRESS_BASE_URL="https://$instance"
  export CYPRESS_RECORD_KEY="$CYPRESS_RECORD_KEY"
  export CYPRESS_RUN_FLAKY="no"

  npm install # get dependencies defined in e2e/package.json

 if ! npm run cypress:run; then
      buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
      exit 1
  fi
done
