#!/bin/bash
configPath=$1certificates
echo $1
echo $configPath


openssl genrsa -out $configPath/root-ca-key.pem 2048
openssl req -new -x509 -sha256 -key $configPath/root-ca-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca" -out $configPath/root-ca.pem -days 1095
# Admin cert
openssl genrsa -out $configPath/admin-key-temp.pem 2048
openssl pkcs8 -inform PEM -outform PEM -in $configPath/admin-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/admin-key.pem
openssl req -new -key $configPath/admin-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin" -out $configPath/admin.csr
openssl x509 -req -in $configPath/admin.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/admin.pem -days 1095
# Node cert 1
openssl genrsa -out $configPath/node1-key-temp.pem 2048
openssl pkcs8 -inform PEM -outform PEM -in $configPath/node1-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/node1-key.pem
openssl req -new -key $configPath/node1-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode" -out $configPath/node1.csr
openssl x509 -req -in $configPath/node1.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/node1.pem -days 1095
# Node cert 2
openssl genrsa -out $configPath/node2-key-temp.pem 2048
openssl pkcs8 -inform PEM -outform PEM -in $configPath/node2-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/node2-key.pem
openssl req -new -key $configPath/node2-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode" -out $configPath/node2.csr
openssl x509 -req -in $configPath/node2.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/node2.pem -days 1095
# Client cert
openssl genrsa -out $configPath/client-key-temp.pem 2048
openssl pkcs8 -inform PEM -outform PEM -in $configPath/client-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/client-key.pem
openssl req -new -key $configPath/client-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefclient" -out $configPath/client.csr
openssl x509 -req -in $configPath/client.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/client.pem -days 1095
# Cleanup
rm $configPath/admin-key-temp.pem
rm $configPath/admin.csr
rm $configPath/node1-key-temp.pem
rm $configPath/node1.csr
rm $configPath/node2-key-temp.pem
rm $configPath/node2.csr
rm $configPath/client-key-temp.pem
rm $configPath/client.csr