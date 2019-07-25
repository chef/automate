#!/bin/bash
set -euo pipefail

error() {
    echo "$*" >&2
}

export VAULT_ADDR=${VAULT_ADDR:-https://vault.chef.co:8200}
export VAULT_CACERT=""
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
echo "Using CHEF_USERNAME=$CHEF_USERNAME"

if [[ ! -f "$HOME/.vault-token" ]]; then
    echo "No cached token found. Attempting to log in."
    echo "Please enter your Chef password:"
    vault login -method=okta username="$CHEF_USERNAME"
else
    echo "Cached token found at $HOME/.vault-token, skipping login"
fi

# A2 license
vault kv get -field=license secret/a2/license > dev/license.jwt

# A1 license
vault kv get -field=license secret/a2/delivery_license | base64 --decode >components/automate-deployment/a1-migration/delivery.license

# Automate acceptence env secrets
target_host=$(vault kv get -field=data secret/a2/testing/target_host)
target_username=$(vault kv get -field=data secret/a2/testing/target_username)
target_user=$(vault kv get -field=data secret/a2/testing/target_user)
target_key=$(vault kv get -field=data secret/a2/testing/target_key)
target_password=$(vault kv get -field=data secret/a2/testing/target_password)
record_key=$(vault kv get -field=record_key secret/a2/cypress)

cat >dev/secrets-env.sh <<EOF
# Secrets
export AUTOMATE_ACCEPTANCE_TARGET_HOST=$target_host
export AUTOMATE_ACCEPTANCE_TARGET_USER=$target_user
export AUTOMATE_ACCEPTANCE_TARGET_KEY="$target_key"
export HAB_STUDIO_SECRET_AUTOMATE_ACCEPTANCE_TARGET_HOST=$target_host
export HAB_STUDIO_SECRET_AUTOMATE_ACCEPTANCE_TARGET_USER=$target_user
export HAB_STUDIO_SECRET_AUTOMATE_ACCEPTANCE_TARGET_KEY="$target_key"
export CYPRESS_AUTOMATE_ACCEPTANCE_TARGET_HOST=$target_host
export CYPRESS_AUTOMATE_ACCEPTANCE_TARGET_USER=$target_user
export CYPRESS_AUTOMATE_ACCEPTANCE_TARGET_KEY="$target_key"
export CYPRESS_RECORD_KEY=$record_key
EOF
