#!/bin/bash

set -eou pipefail

VERSION=$(cat VERSION)
MAJOR_VERSION=${VERSION%%\.*}
export MAJOR_VERSION
export VERSION

log "Using VERSION=$VERSION"

# Export the HAB_AUTH_TOKEN for use of promoting habitat packages to {{TARGET_CHANNE}}
HAB_AUTH_TOKEN=$(vault kv get -field auth_token account/static/habitat/chef-ci)
export HAB_AUTH_TOKEN

source_channel=$EXPEDITOR_PROMOTABLE

# Download the manifest
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/manifest_semver.json" manifest.json --profile chef-cd

# Pull the version from the manifest
version=$(jq -r -c ".version" manifest.json)

# Import packages@chef.io GPG signing key
aws s3 cp s3://chef-cd-citadel/packages_at_chef.io.pgp packages_at_chef.io.pgp --profile=chef-cd
gpg --import packages_at_chef.io.pgp

# Creating the Automate Airgap Bundle
"${automate_cli_path}/static/linux/chef-automate" airgap bundle create -c dev
airgapbundle=`ls | grep automate-[0-9.]*aib`

# Create gpg signature and sha256sum
gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "$airgapbundle.asc" --detach-sign $airgapbundle
sha256sum $airgapbundle > "$airgapbundle.sha256sum"
ls

# Upload the bundle to S3 Bucket
aws s3 cp $airgapbundle "s3://chef-automate-artifacts/airgap_bundle/$VERSION/$airgapbundle" --acl public-read --profile chef-cd
aws s3 cp $airgapbundle.asc "s3://chef-automate-artifacts/airgap_bundle/$VERSION/$airgapbundle.asc" --acl public-read --profile chef-cd
aws s3 cp $airgapbundle.sha256sum "s3://chef-automate-artifacts/airgap_bundle/$VERSION/$airgapbundle.sha256sum" --acl public-read --profile chef-cd
aws s3 cp $airgapbundle "s3://chef-automate-artifacts/acceptance/latest/automate/airgap_bundle/automate.aib" --acl public-read --profile chef-cd
aws s3 cp $airgapbundle.asc "s3://chef-automate-artifacts/acceptance/latest/automate/airgap_bundle/automate.asc" --acl public-read --profile chef-cd
aws s3 cp $airgapbundle.sha256sum "s3://chef-automate-artifacts/acceptance/latest/automate/airgap_bundle/automate.sha256sum" --acl public-read --profile chef-cd

# Cleanup
rm manifest.json