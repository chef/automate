#!/bin/bash
basePath=$1
echo " ================= Getting certs from vault using ==============="
echo $OPENSEARCH_ROOT_CA_PEM
echo $OPENSEARCH_ADMIN_PEM
echo $OPENSEARCH_ADMIN_KEY_PEM
echo $OPENSEARCH_NODE1_PEM
echo $OPENSEARCH_NODE1_KEY_PEM

touch $basePath/default_backend_certificates.toml

cat <<EOF >> $basePath/default_backend_certificates.toml
# root pem cert that signed the two cert/key pairs below
rootCA = """$OPENSEARCH_ROOT_CA_PEM"""
EOF
 
cat <<EOF >> $basePath/default_backend_certificates.toml
# Certificate used for admin actions against https://9200
admin_cert = """$OPENSEARCH_ADMIN_PEM"""
EOF
 
cat <<EOF >> $basePath/default_backend_certificates.toml
# the private key associated with the above pem cert
admin_key = """$OPENSEARCH_ADMIN_KEY_PEM"""
EOF
 
cat <<EOF >> $basePath/default_backend_certificates.toml
# Certificate used for intracluster ssl on port 9300
ssl_cert = """$OPENSEARCH_NODE1_PEM"""
EOF
 
cat <<EOF >> $basePath/default_backend_certificates.toml
# the private key associated with the above pem cert
ssl_key = """$OPENSEARCH_NODE1_KEY_PEM"""
EOF
