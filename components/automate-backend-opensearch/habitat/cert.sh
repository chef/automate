#!/bin/bash

# configPath=$1/os/config/certificates
# basePath=$2
# echo $configPath
# 
# openssl genrsa -out $configPath/root-ca-key.pem 2048
# openssl req -new -x509 -sha256 -key $configPath/root-ca-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefrootca" -out $configPath/root-ca.pem -days 1095
# # Admin cert
# openssl genrsa -out $configPath/admin-key-temp.pem 2048
# openssl pkcs8 -inform PEM -outform PEM -in $configPath/admin-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/admin-key.pem
# openssl req -new -key $configPath/admin-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefadmin" -out $configPath/admin.csr
# openssl x509 -req -in $configPath/admin.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/admin.pem -days 1095
# # Node cert 1
# openssl genrsa -out $configPath/node1-key-temp.pem 2048
# openssl pkcs8 -inform PEM -outform PEM -in $configPath/node1-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/node1-key.pem
# openssl req -new -key $configPath/node1-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode" -out $configPath/node1.csr
# openssl x509 -req -in $configPath/node1.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/node1.pem -days 1095
# # Node cert 2
# openssl genrsa -out $configPath/node2-key-temp.pem 2048
# openssl pkcs8 -inform PEM -outform PEM -in $configPath/node2-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/node2-key.pem
# openssl req -new -key $configPath/node2-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefnode" -out $configPath/node2.csr
# openssl x509 -req -in $configPath/node2.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/node2.pem -days 1095
# # Client cert
# openssl genrsa -out $configPath/client-key-temp.pem 2048
# openssl pkcs8 -inform PEM -outform PEM -in $configPath/client-key-temp.pem -topk8 -nocrypt -v1 PBE-SHA1-3DES -out $configPath/client-key.pem
# openssl req -new -key $configPath/client-key.pem -subj "/C=US/ST=Washington/L=Seattle/O=Chef Software Inc/CN=chefclient" -out $configPath/client.csr
# openssl x509 -req -in $configPath/client.csr -CA $configPath/root-ca.pem -CAkey $configPath/root-ca-key.pem -CAcreateserial -sha256 -out $configPath/client.pem -days 1095
# # Cleanup   
# rm $configPath/admin-key-temp.pem
# rm $configPath/admin.csr
# rm $configPath/node1-key-temp.pem
# rm $configPath/node1.csr
# rm $configPath/node2-key-temp.pem
# rm $configPath/node2.csr
# rm $configPath/client-key-temp.pem
# rm $configPath/client.csr


echo "======================= Installing Vault ============================="

export VAULT_ADDR=${VAULT_ADDR:-https://vault.ps.chef.co}
export VAULT_CACERT=""
export VAULT_NAMESPACE=root
CHEF_USERNAME=${CHEF_USERNAME:-unknown}
STUDIO_TYPE=${STUDIO_TYPE:-none}

if [[ "$CHEF_USERNAME" = "unknown" ]]; then
    error "CHEF_USERNAME not set. This command will only work for people with
access to the Chef Software VPN. If you are not a Chef Software
employee and require one of the secrets retrieved by this
script, please open a GitHub issue."
    exit 1
fi

# Install vault and other required packages
if ! command -v vault >/dev/null; then
    if [[ "$STUDIO_TYPE" == "none" ]];then
        error "vault is not installed and is required for this script.
If you run this command in the studio, vault will be installed automatically.
Otherwise, please install vault on your local workstation from:

    https://www.vaultproject.io/downloads.html
"
        exit 1
    else
        hab pkg install core/vault -b -f
        hab pkg install core/cacerts
    fi
fi

if [[ "$STUDIO_TYPE" != "none" ]];then
    VAULT_CACERT="$(hab pkg path core/cacerts)/ssl/certs/cacert.pem"
    export VAULT_CACERT
fi

echo "Using VAULT_ADDR=$VAULT_ADDR"
echo "Using VAULT_CACERT=$VAULT_CACERT"
echo "Using VAULT_NAMESPACE=$VAULT_NAMESPACE"
echo "Using CHEF_USERNAME=$CHEF_USERNAME"

if [[ ! -f "$HOME/.vault-token" ]]; then
    echo "No cached token found. Attempting to log in."
    echo "Please enter your Chef password:"
    vault login -method=okta username="$CHEF_USERNAME"
else
    echo "Cached token found at $HOME/.vault-token, skipping login"
fi


ROOT_CA_PEM=$(vault kv get  -field=root-ca.pem secret/a2/a2ha/opensearch)
echo ROOT_CA_PEM
ADMIN_PEM=$(vault kv get  -field=admin.pem secret/a2/a2ha/opensearch)
echo ADMIN_PEM
ADMIN_KEY_PEM=$(vault kv get  -field=admin-key.pem secret/a2/a2ha/opensearch)
echo ADMIN_KEY_PEM
NODE1_PEM=$(vault kv get  -field=node1.pem secret/a2/a2ha/opensearch)
echo NODE1_PEM
NODE1_KEY_PEM=$(vault kv get  -field=node1-key.pem secret/a2/a2ha/opensearch)
echo NODE1_KEY_PEM


cat <<EOF >> $basePath/default.toml
# root pem cert that signed the two cert/key pairs below
rootCA = """$ROOT_CA_PEM"""
EOF
 
cat <<EOF >> $basePath/default.toml
# Certificate used for admin actions against https://9200
admin_cert = """$ADMIN_PEM"""
EOF
 
cat <<EOF >> $basePath/default.toml
# the private key associated with the above pem cert
admin_key = """$ADMIN_KEY_PEM"""
EOF
 
cat <<EOF >> $basePath/default.toml
# Certificate used for intracluster ssl on port 9300
ssl_cert = """$NODE1_PEM"""
EOF
 
cat <<EOF >> $basePath/default.toml
# the private key associated with the above pem cert
ssl_key = """$NODE1_KEY_PEM"""
EOF
