#!/bin/bash

set -eou pipefail

if [ "${EXPEDITOR_TARGET_CHANNEL}" = "unstable" ];
then
  echo "This file does not support actions for artifacts promoted to unstable"
  exit 1
fi

#
# The following standard Expeditor artifact_actions environment variables
# are automatically set by the calling process:
#
# PROMOTABLE - in promotion artifact actions this is a reference to the
#              source channel.
# TARGET_CHANNEL - the channel which we are promoting to
# HAB_AUTH_TOKEN - GitHub Auth token used to communicate with the
#                  Habitat depot and private repos in Chef's GitHub org
#

source_channel=$EXPEDITOR_PROMOTABLE

# Download the manifest
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/manifest.json" manifest.json --profile chef-cd

# Download or create the versions file
aws s3 cp "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/versions.json" existing-versions.json --profile chef-cd || echo "[]" > existing-versions.json

# Download or create the releases file
aws s3 cp "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/releases.json" existing-releases.json --profile chef-cd || echo "[]" > existing-releases.json

# Pull the version from the manifest
version=$(jq -r -c ".build" manifest.json)

# Promote the artifacts in Habitat Depot
jq -r -c ".packages[]" manifest.json | while read -r service_ident; do
  pkg_origin=${service_ident%/*/*/*}

  if [ "$pkg_origin" = "core" ];
  then
    echo "Skipping promotion of core origin package ${service_ident}"
  else
    echo "Promoting ${service_ident} to the ${EXPEDITOR_TARGET_CHANNEL} channel"
    hab pkg promote "${service_ident}" "${EXPEDITOR_TARGET_CHANNEL}"
  fi
done

# Append the new version to the target channel versions file
jq ". |= .+ [\"$version\"]" existing-versions.json > updated-versions.json

# Append the new release to the dev channel releases file
release_date=$(date -u +"%Y-%m-%dT%H:%M:%S+00:00")
release_notes="https://packages.chef.io/release-notes/automate/$version.md"
manifest="https://packages.chef.io/manifests/automate/$version.json"
licenses="https://packages.chef.io/licenses/automate/$version.json"
release=$(printf '{"version":"%s","release_date":"%s","_links":{"release_notes":"%s","manifest":"%s","licenses":"%s"}}' "$version" "$release_date" "$release_notes" "$manifest" "$licenses")
jq ". |= .+ [$release]" existing-releases.json > updated-releases.json

# Promote the CLI
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/chef-automate_linux_amd64.zip" "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/chef-automate_linux_amd64.zip" --acl public-read  --profile chef-cd
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/chef-automate_linux_amd64.zip.asc" "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/chef-automate_linux_amd64.zip.asc" --acl public-read  --profile chef-cd
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/chef-automate_linux_amd64.zip.sha256sum" "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/chef-automate_linux_amd64.zip.sha256sum" --acl public-read  --profile chef-cd

# Promote the License Scout Dependency Manifest
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/licenses.json" "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/licenses.json" --acl public-read  --profile chef-cd

# Promote the manifest
aws s3 cp manifest.json "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/manifest.json" --acl public-read  --profile chef-cd
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/manifest.json.asc" "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/manifest.json.asc" --acl public-read  --profile chef-cd
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/manifest.json.sha256sum" "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/manifest.json.sha256sum" --acl public-read  --profile chef-cd

# Upload the updated versions file
aws s3 cp updated-versions.json "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/versions.json" --acl public-read  --profile chef-cd

# Upload the updated releases file
aws s3 cp updated-releases.json "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/releases.json" --acl public-read --profile chef-cd

# Cleanup
rm manifest.json
rm existing-versions.json
rm existing-releases.json
rm updated-versions.json
rm updated-releases.json
