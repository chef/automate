#!/bin/bash

set -euo pipefail

ng_env=${NG_ENV:-dev}
dns_suffix=${DNS_SUFFIX:-dev}

# cwd is ./terraform/test-environments
pushd ../../components/release-mgmt-dashboard
  npm install
  ng build --prod --env=$ng_env
  aws --profile chef-cd s3 sync dist s3://a2-$dns_suffix.cd.chef.co
popd
