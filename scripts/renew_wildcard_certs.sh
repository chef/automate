#!/usr/bin/env bash
set -Eeo pipefail
CERT_DIR="$(pwd)/etc/letsencrypt/live/cd.chef.co/"

docker run -p 80:80 -p 443:443 \
           -it --rm --name certbot \
           -e AWS_CONFIG_FILE=/root/.aws/config \
           -e AWS_PROFILE=chef-cd \
           -v "/var/lib/buildkite-agent/.aws:/root/.aws" \
           -v "$(pwd)/etc/letsencrypt:/etc/letsencrypt" \
           -v "/var/lib/letsencrypt:/var/lib/letsencrypt" \
           certbot/dns-route53 certonly \
           --dns-route53 \
           -d "*.cd.chef.co" \
           --register-unsafely-without-email \
           --agree-tos

sudo chown -R "$(id -u "$USER")":"$(id -g "$USER")" "$(pwd)/etc/"

if [[ -f "${CERT_DIR}/fullchain.pem" && -f "${CERT_DIR}/privkey.pem" ]]; then
    echo "Updating certificates in vault..."
    cd "${CERT_DIR}"
    vault write secret/a2/testing/wildcard_cert crt=@fullchain.pem key=@privkey.pem
else
    echo "Certificates not found. Skipping vault update step"
fi
