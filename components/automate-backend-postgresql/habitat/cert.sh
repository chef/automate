#!/bin/bash
basePath=$1
echo " ================= Getting certs from vault using =============="
echo $OPENSEARCH_ROOT_CA_PEM
echo $OPENSEARCH_ADMIN_PEM
echo $OPENSEARCH_ADMIN_KEY_PEM
echo $OPENSEARCH_NODE1_PEM
echo $OPENSEARCH_NODE1_KEY_PEM

cat <<EOF >> habitat/default.toml
# server public cert used for ssl listener
ssl_cert = """$OPENSEARCH_NODE1_PEM"""
EOF
 
cat <<EOF >> habitat/default.toml
# server private key
ssl_key = """$OPENSEARCH_NODE1_KEY_PEM"""
EOF

cat <<EOF >> habitat/default.toml
# issuer public cert that signed the above server public cert
issuer_cert = """$OPENSEARCH_ROOT_CA_PEM"""
EOF