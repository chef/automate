#!/bin/bash

export HAB_LICENSE=accept-no-persist

set -euo pipefail

hab ring key generate automate --cache-key-path . >/dev/null
sym_key=$(find . -iname "automate-*sym.key" | head -1)
key_content=$(cat < "$sym_key" | sed s/$/\\\\n/ | tr -d '\n' | sed s/\\\\n\$//)
openssl_cmd=$(hab pkg path core/openssl)/bin/openssl
bearer_token=$($openssl_cmd rand -base64 32)
echo "hab_sup_http_gateway_auth_token = \"$bearer_token\""
echo "hab_sup_ring_key = \"$key_content\""
rm -f "$sym_key"
