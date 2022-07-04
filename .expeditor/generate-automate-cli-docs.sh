#!/bin/bash
set -eou pipefail

# bumping expeditor to go 1.18.3
hab pkg install --binlink core/go/1.18.3 --force

pushd components/automate-cli
  make docs
popd
