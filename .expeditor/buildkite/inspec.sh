#!/bin/bash

set -euo pipefail

echo -e "$CHEF_CI_SSH_PRIVATE_KEY" > chef-ci-ad-ssh

# TODO(sr) Until we've fixed the inspec resource to deal with a single
# login method properly, let's skip those machines for inspec tests.
instances_to_test=$(curl --silent "https://a2-${CHANNEL}.cd.chef.co/assets/data.json" |\
  jq --raw-output '.[] | select(.tags | any(. == "chef-automate-cli")) | select(.tags | any(. == "saml")) | .fqdn')

for instance in ${instances_to_test[*]}
do
  echo "--- Executing a2-deploy-smoke profile against $instance"

  cat >./attrs.yml <<EOH
---
target_host: $instance
EOH

  inspec exec inspec/a2-deploy-smoke --sudo --target "ssh://chef-ci@$instance" -i chef-ci-ad-ssh --attrs attrs.yml
done
