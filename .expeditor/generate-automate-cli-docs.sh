#!/bin/bash
set -eou pipefail

# bumping expeditor to go 1.22
hab pkg install --binlink core/go22 --force

pushd components/automate-cli
  make docs
popd
