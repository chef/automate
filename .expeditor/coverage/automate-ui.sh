#!/bin/bash

set -xeuo pipefail

export COVERAGE_DATE=$(date +"%Y-%m-%dT%H:%M:%S")

pushd ./components/automate-ui
  npm install
  # https://github.com/sass/node-sass/issues/1579
  # We were failing with a missing node-sass file and that bug recommended to rebuild it - fixed the error we were seeing
  npm rebuild node-sass
  npm run test
  aws --profile chef-cd s3 sync coverage s3://a2-code-coverage.cd.chef.co/automate-ui/current
  aws --profile chef-cd s3 sync coverage "s3://a2-code-coverage.cd.chef.co/automate-ui/$COVERAGE_DATE"
popd

pushd ./.expeditor/coverage
./generate-index.rb
popd
