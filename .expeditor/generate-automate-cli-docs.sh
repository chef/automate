#!/bin/bash

set -eou pipefail

mkdir -p /go/src/github.com/chef
ln -s /workspace /go/src/github.com/chef/automate

# bumping expeditor to go 1.13
hab pkg install --binlink core/go/1.13.5 --force

pushd /go/src/github.com/chef/automate/components/automate-cli
  make docs
popd
