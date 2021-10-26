#!/usr/bin/env bash
CERT_DIR="$(pwd)/etc/letsencrypt/live/cd.chef.co/"

credentials=$(vault read account/dynamic/aws/chef-cd/creds/default --format=json | jq '.data')
session=$(echo $credentials | jq '.security_token')
access_key=$(echo $credentials | jq '.access_key')
secret_key=$(echo $credentials | jq '.secret_key')

docker run -p 80:80 -p 443:443 \
           -it --rm --name certbot \
           -e AWS_SESSION_TOKEN=${session} \
           -e AWS_ACCESS_KEY_ID=${access_key} \
           -e AWS_SECRET_ACCESS_KEY=${secret_key} \ 
           -v "$(pwd)/etc/letsencrypt:/etc/letsencrypt" \
           -v "/var/lib/letsencrypt:/var/lib/letsencrypt" \
           certbot/dns-route53 certonly \
           --dns-route53 \
           -d "*.cd.chef.co" \
           --register-unsafely-without-email \
           --agree-tos

if [[ -f "${CERT_DIR}/fullchain.pem" && -f "${CERT_DIR}/privkey.pem" ]]; then
    echo "Updating certificates in vault..."
    cd ${CERT_DIR}
    vault write secret/mike foo=@fullchain.pem bar=privkey.pem
else
    echo "Certificates not found. Skipping vault update step"
fi