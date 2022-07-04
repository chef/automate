#!/bin/bash
set -eou pipefail

# bumping expeditor to go 1.18
hab pkg install --binlink core/go/1.18 --force 

pushd components/automate-cli
  make docs
popd
