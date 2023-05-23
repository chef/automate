#!/bin/bash

cert_path="$1"

# Check for empty certificate path
if [ -z "$cert_path" ]; then
    echo "Error: Certificate path not provided"
    echo "usage:"
    echo "bash san_validator.sh <certificate path>"
    exit 1
fi

# Check for the certificate file exists
if [ ! -f "$cert_path" ]; then
    echo "Error: Certificate file not found."
    exit 1
fi

# Get the certificate information
cert_info=$(openssl x509 -in "$cert_path" -noout -text 2>/dev/null)

# Check if the certificate has SAN or not
if echo "$cert_info" | grep -q "Subject Alternative Name"; then
    echo "Pass: your certificate is a valid SAN certificate"
else
    echo "Fail: your certificate is not a valid SAN certificate"
fi