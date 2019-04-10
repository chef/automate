#!/bin/bash

set -euo pipefail

echo -e "$CHEF_CI_SSH_PRIVATE_KEY" > chef-ci-ad-ssh

instances_to_test=$(curl --silent https://a2-${CHANNEL}.cd.chef.co/assets/data.json | jq --raw-output 'map(select(.tags[] | contains ("chef-automate-cli"))) | .[] .fqdn')

for instance in ${instances_to_test[*]}
do
  echo "--- Executing a2-deploy-smoke profile against $instance"

  cat >./attrs.yml <<EOH
---
target_host: $instance
EOH

  inspec exec inspec/a2-deploy-smoke --sudo --target ssh://chef-ci@$instance -i chef-ci-ad-ssh --attrs attrs.yml
done
