#!/bin/bash

set -eou pipefail

target_channel="${EXPEDITOR_TARGET_CHANNEL:-dev}"

echo "Purging '${target_channel}/automate/latest' Surrogate Key group from Fastly"
curl -X POST -H "Fastly-Key: $FASTLY_API_TOKEN" "https://api.fastly.com/service/1ga2Kt6KclvVvCeUYJ3MRp/purge/${target_channel}/automate/latest"
