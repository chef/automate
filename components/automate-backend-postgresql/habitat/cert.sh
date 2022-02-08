#!/bin/bash
 
openssl genrsa -out MyRootCA.key 2048
 
openssl req -x509 -new -days 1095 -key MyRootCA.key -sha256 -out MyRootCA.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'
 
openssl genrsa -out ssl-pkcs12.key 2048
 
openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out "ssl.key" -nocrypt
 
openssl req -new -key ssl.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'
 
openssl x509 -days 1095 -req -in ssl.csr -CA MyRootCA.pem -CAkey MyRootCA.key -CAcreateserial -out ssl.pem -sha256
 
cat <<EOF >> habitat/default.toml
# server public cert used for ssl listener
ssl_cert = """$(cat ssl.pem)"""
EOF
 
cat <<EOF >> habitat/default.toml
# server private key
ssl_key = """$(cat ssl.key)"""
EOF

cat <<EOF >> habitat/default.toml
# issuer public cert that signed the above server public cert
issuer_cert = """$(cat MyRootCA.pem)"""
EOF