#!/bin/bash
set -eou pipefail

# bumping expeditor to go 1.19
hab pkg install --binlink core/go19 --force

pushd components/automate-cli
  make docs
popd
