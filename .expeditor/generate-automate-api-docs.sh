#!/bin/bash

set -eou pipefail

mkdir -p /go/src/github.com/chef
ln -s /workspace /go/src/github.com/chef/automate

pushd /go/src/github.com/chef/automate/components/automate-chef-io
  make sync_swagger_files
popd
