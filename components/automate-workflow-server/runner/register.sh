#!/bin/bash
set -ex

ADMIN_PASS="${BASH_ARGV[0]}"
TOKEN=$(curl -v http://delivery/workflow/api/v0/e/cd/users/admin/get-token \
  -H "Content-type: application/json" \
  -d "{\"username\": \"admin\", \"password\": \"${ADMIN_PASS}\"}" | awk -F\" '/token/{print $4}')

PUBKEY=$(curl -v http://delivery/workflow/api/v0/e/cd/runners \
  -H "Content-type: application/json" \
  -H "chef-delivery-user: admin" \
  -H "chef-delivery-token: $TOKEN" \
  -d "{\"hostname\": \"workflow_runner\", \"os\": \"linux\", \"platform_family\": \"debian\", \"platform\": \"ubuntu\", \"platform_version\": \"14.04\"}" |
  sed -n 's/^.*"openssh_public_key":"\(.*\)\\n",.*}$/\1/p')

echo $PUBKEY > ~job_runner/.ssh/authorized_keys
