#!/bin/bash
set -euo pipefail

pushd ./components/chef-ui-library
  npm install
  npm run docs
  npm run build
  aws --profile chef-cd s3 sync www s3://ui-library.cd.chef.co
popd
