#!/bin/bash

set -euo pipefail

export CHANNEL="${EXPEDITOR_TARGET_CHANNEL:-dev}"

cd terraform/automate-chef-io-deploy
make apply
