#!/bin/bash
 
openssl genrsa -out MyRootCA.key 2048

openssl req -x509 -new -days 1095 -key MyRootCA.key -sha256 -out MyRootCA.pem -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca'

openssl genrsa -out admin-pkcs12.key 2048
 
openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "admin-pkcs12.key" -topk8 -out "admin.key" -nocrypt
 
openssl req -new -key admin.key -out admin.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin'
 
openssl x509 -days 1095 -req -in admin.csr -CA MyRootCA.pem -CAkey MyRootCA.key -CAcreateserial -out admin.pem -sha256
 
openssl genrsa -out ssl-pkcs12.key 2048
 
openssl pkcs8 -v1 "PBE-SHA1-3DES" -in "ssl-pkcs12.key" -topk8 -out "ssl.key" -nocrypt
 
openssl req -new -key ssl.key -out ssl.csr -subj '/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode'
 
openssl x509 -days 1095 -req -in ssl.csr -CA MyRootCA.pem -CAkey MyRootCA.key -CAcreateserial -out ssl.pem -sha256
 
cat <<EOF >> habitat/default.toml
# root pem cert that signed the two cert/key pairs below
rootCA = """$(cat MyRootCA.pem)"""
EOF
 
cat <<EOF >> habitat/default.toml
# Certificate used for admin actions against https://9200
admin_cert = """$(cat admin.pem)"""
EOF
 
cat <<EOF >> habitat/default.toml
# the private key associated with the above pem cert
admin_key = """$(cat admin.key)"""
EOF
 
cat <<EOF >> habitat/default.toml
# Certificate used for intracluster ssl on port 9300
ssl_cert = """$(cat ssl.pem)"""
EOF
 
cat <<EOF >> habitat/default.toml
# the private key associated with the above pem cert
ssl_key = """$(cat ssl.key)"""
EOF