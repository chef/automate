#!/bin/bash

set -eou pipefail

EXPEDITOR_TARGET_CHANNEL="acceptance"
if [ "${EXPEDITOR_TARGET_CHANNEL}" = "unstable" ];
then
  echo "This file does not support actions for artifacts promoted to unstable"
  exit 1
fi

aws s3 cp "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/manifest_semver.json" manifest.json --profile chef-cd

# Pull the version from the manifest
version=$(jq -r -c ".version" manifest.json)

# Get the current cli from the channel
curl https://packages.chef.io/files/"${EXPEDITOR_TARGET_CHANNEL}"/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate

./chef-automate airgap bundle create -c "${EXPEDITOR_TARGET_CHANNEL}" ./"${version}".aib

gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "$airgapbundle.asc" --detach-sign ./"${version}".aib
sha256sum "$airgapbundle" > "${version}".aib.sha256sum

aws s3 cp "${version}".aib "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/latest.aib" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.asc "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/latest.aib.asc" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.sha256sum "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/latest.aib.sha255sum" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/${version}/${version}.aib" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.asc "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/${version}/${version}.aib.asc" --acl public-read --profile chef-cd
aws s3 cp "${version}".aib.sha256sum "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/${version}/${version}.aib.sha255sum" --acl public-read --profile chef-cd
