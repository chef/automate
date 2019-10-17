#!/bin/bash

set -euo pipefail

cd /workdir/e2e

data=$(curl --silent "https://a2-${CHANNEL}.cd.chef.co/assets/data.json")
instances_to_test=$(jq -nr --argjson data "$data" '$data[] | select(.tags | any(. == "e2e")) | .fqdn')
versions=(v1 v2.1)
timestamp=$(date +"%m-%d-%y-%H-%M")

for instance in ${instances_to_test[*]}
do
  if ! jq -enr --arg fqdn "$instance" --argjson data "$data" '$data[] | select(.fqdn == $fqdn) | .tags | any(. == "saml")'; then
    export CYPRESS_SKIP_SSO=true
  fi

  for version in ${versions[*]}
  do
    if jq -enr --arg version "$version" --arg fqdn "$instance" --argjson data "$data" '$data[] | select(.fqdn == $fqdn) | .tags | any(. == $version)'; then
      if [[ "$version" == "v1" ]]; then
        if ! token=$(chef-automate admin-token); then
          echo "Non-zero exit code, output:"
          echo "$token"
          return 1
        fi
      else
        if ! token=$(chef-automate iam token create "$timestamp-tok" --admin); then
          echo "Non-zero exit code, output:"
          echo "$token"
          return 1
        fi
      fi

      export CYPRESS_ADMIN_TOKEN=$token
      export CYPRESS_IAM_VERSION=$version
    fi
  done

  echo "--- Executing Cypress tests against $instance"
  export CYPRESS_BASE_URL="https://$instance"
  export CYPRESS_RECORD_KEY="$CYPRESS_RECORD_KEY"

  npm install # get dependencies defined in e2e/package.json

  if ! npm run cypress:record; then
      buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
      exit 1
  fi
done
