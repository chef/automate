#!/bin/bash
set -eou pipefail

# bumping expeditor to go 1.22.5
hab pkg install --binlink core/go1_22/1.22.5/20240805184444 --force

pushd components/automate-cli
  make docs
popd
