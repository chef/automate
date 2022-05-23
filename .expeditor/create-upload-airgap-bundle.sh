#!/bin/bash

set -eou pipefail

if [ "${EXPEDITOR_TARGET_CHANNEL}" = "unstable" ];
then
  echo "This file does not support actions for artifacts promoted to unstable"
  exit 1
fi

# Import packages@chef.io GPG signing key
aws s3 cp s3://chef-cd-citadel/packages_at_chef.io.pgp packages_at_chef.io.pgp --profile=chef-cd
gpg --import packages_at_chef.io.pgp

# Download the manifest
aws s3 cp "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/manifest_semver.json" manifest.json --profile chef-cd

# Pull the version from the manifest
version=$(jq -r -c ".version" manifest.json)

# Get the current cli from the channel
curl https://packages.chef.io/files/"${EXPEDITOR_TARGET_CHANNEL}"/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate

# Creating the airgap bundle
./chef-automate airgap bundle create -c "${EXPEDITOR_TARGET_CHANNEL}" ./"${version}".aib

# Create gpg signature and sha256sum
gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "${version}".aib.asc --detach-sign ./"${version}".aib
sha256sum "${version}".aib > "${version}".aib.sha256sum

# Uploading the airgap bundle to s3 locations
aws s3 cp "${version}".aib "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/airgap_bundle/automate.aib" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.asc "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest//automate/airgap_bundle/automate.aib.asc" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.sha256sum "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/airgap_bundle/automate.aib.sha256sum" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/airgap_bundle/${version}/${version}.aib" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.asc "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/airgap_bundle/${version}/${version}.aib.asc" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.sha256sum "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/airgap_bundle/${version}/${version}.aib.sha256sum" --acl public-read --profile chef-cd
