#!/bin/bash
 
openssl genrsa -out MyRootCA.key 2048
 
openssl req -x509 -new -key MyRootCA.key -sha256 -out MyRootCA.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'
 
openssl genrsa -out ssl-pkcs12.key 2048
 
openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out "ssl.key" -nocrypt
 
openssl req -new -key ssl.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'
 
openssl x509 -req -in ssl.csr -CA MyRootCA.pem -CAkey MyRootCA.key -CAcreateserial -out ssl.pem -sha256

cat <<EOF >> default.toml
# Certificate used for intracluster ssl on port 9300
ssl_cert = """$(cat ssl.pem)"""
EOF
 
cat <<EOF >> default.toml
# the private key associated with the above pem cert
ssl_key = """$(cat ssl.key)"""
EOF

