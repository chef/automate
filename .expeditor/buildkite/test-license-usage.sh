#!/bin/bash

set -euo pipefail

# This block translates the "channel" into the appropriate set of VPC settings
# used in terraform/Makefile. These settings still rely on old Workflow-isms,
# which is why the TF_ENVIRONMENT name doesn't match up with the CHANNEL.
if [ "$CHANNEL" == "unstable" ]; then
  export TF_ENVIRONMENT=acceptance
elif [ "$CHANNEL" == "dev" ]; then
  export TF_ENVIRONMENT=union
elif [ "$CHANNEL" == "acceptance" ]; then
  export TF_ENVIRONMENT=delivered
else
  puts "We do not currently support deploying channel $CHANNEL"
  exit 1
fi

export LOGS_URL="$BUILDKITE_BUILD_URL"

cd terraform/test-license-usage
make apply
