#!/usr/bin/env sh
#
# NOTE: This script is likely sourced in /bin/sh and not bash. Please
# keep code in this script sh-compatible.
#

echo "credential length before encoding:"
echo -n "$GOOGLE_CREDS_JSON" | wc -c

encoded_creds=$(base64 <<<"$GOOGLE_CREDS_JSON")
export GOOGLE_CREDS_JSON=$encoded_creds

echo "encoded GCP credential"
echo "credential length after encoding:"
echo -n "$GOOGLE_CREDS_JSON" | wc -c
