#!/bin/bash -e

cat > v3.txt <<- EOF
keyUsage = critical, digitalSignature, keyEncipherment, dataEncipherment, keyAgreement
extendedKeyUsage = serverAuth, clientAuth
subjectKeyIdentifier=hash
authorityKeyIdentifier=keyid,issuer
subjectAltName = IP:127.0.0.1,DNS:deployment-service
EOF

openssl req -x509 \
  -subj "/C=US/ST=WA/L=Seattle/O=Chef/OU=Dev/CN=Chef Automate" \
  -nodes \
  -newkey rsa:4096 \
  -days 1826 \
  -keyout root.key \
  -out root.crt
openssl genrsa -out deployment-service.key 4096
openssl req -new \
  -key deployment-service.key \
  -subj "/CN=deployment-service" \
  -reqexts SAN \
  -config <(cat /etc/ssl/openssl.cnf \
      <(printf "\n[SAN]\nsubjectAltName=DNS:deployment-service,IP:127.0.0.1")) \
  -sha256 \
  -out deployment-service.csr
openssl x509 -req \
  -extfile v3.txt \
  -CA root.crt \
  -CAkey root.key \
  -CAcreateserial \
  -days 1825 \
  -sha256 \
  -in deployment-service.csr \
  -out good.crt

rm v3.txt deployment-service.csr deployment-service.key root.key root.srl
