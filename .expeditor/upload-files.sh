#!/bin/bash

set -eou pipefail

VERSION=$(date +"%Y%m%d%H%M%S")
export VERSION
# Should ensure License Scout doesn't get rate limited
OCTOKIT_ACCESS_TOKEN=$GITHUB_TOKEN
export OCTOKIT_ACCESS_TOKEN

# Import packages@chef.io GPG signing key
aws s3 cp s3://chef-cd-citadel/packages_at_chef.io.pgp packages_at_chef.io.pgp --profile=chef-cd
gpg --import packages_at_chef.io.pgp

# Generate the License Scout Dependency Manifest
.expeditor/license_scout.sh

# Upload the License Scout Dependency Manifest to the S3 bucket
aws s3 cp a2-dependency-licenses.json "s3://chef-automate-artifacts/licenses/automate/$VERSION.json" --acl public-read --profile chef-cd
aws s3 cp a2-dependency-licenses.json s3://chef-automate-artifacts/dev/latest/automate/licenses.json --acl public-read --profile chef-cd

#
# Generate the manifest.json
#

# Download or create the versions file
aws s3 cp "s3://chef-automate-artifacts/dev/latest/automate/versions.json" existing-versions.json --profile chef-cd || echo "[]" > existing-versions.json

# Download or create the releases file
aws s3 cp "s3://chef-automate-artifacts/dev/latest/automate/releases.json" existing-releases.json --profile chef-cd || echo "[]" > existing-releases.json

# Use create-manifest to generate the manifest.json file
ruby .expeditor/create-manifest.rb

# Append the new version to the dev channel versions file
jq ". |= .+ [\"$VERSION\"]" existing-versions.json > updated-versions.json

# Append the new release to the dev channel releases file
release_date=$(date -u +"%Y-%m-%dT%H:%M:%S+00:00")
release_notes="https://packages.chef.io/release-notes/automate/$VERSION.md"
manifest="https://packages.chef.io/manifests/automate/$VERSION.json"
licenses="https://packages.chef.io/licenses/automate/$VERSION.json"
release=$(printf '{"version":"%s","release_date":"%s","_links":{"release_notes":"%s","manifest":"%s","licenses":"%s"}}' "$VERSION" "$release_date" "$release_notes" "$manifest" "$licenses")
jq ". |= .+ [$release]" existing-releases.json > updated-releases.json

# Upload the manifest to the S3 bucket
aws s3 cp "$VERSION.json" "s3://chef-automate-artifacts/manifests/automate/$VERSION.json" --acl public-read --profile chef-cd
aws s3 cp "$VERSION.json" s3://chef-automate-artifacts/dev/latest/automate/manifest.json --acl public-read --profile chef-cd
aws s3 cp updated-versions.json s3://chef-automate-artifacts/dev/latest/automate/versions.json --acl public-read --profile chef-cd
aws s3 cp updated-releases.json s3://chef-automate-artifacts/dev/latest/automate/releases.json --acl public-read --profile chef-cd

# Create gpg signature and sha256sum of the manifest
gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "$VERSION.json.asc" --detach-sign "$VERSION.json"
sha256sum "$VERSION.json" > "$VERSION.json.sha256sum"

# Upload the manifest's gpg signature and sha256sum to the S3 bucket
aws s3 cp "$VERSION.json.asc" "s3://chef-automate-artifacts/manifests/automate/$VERSION.json.asc" --acl public-read --profile chef-cd
aws s3 cp "$VERSION.json.sha256sum" "s3://chef-automate-artifacts/manifests/automate/$VERSION.json.sha256sum" --acl public-read --profile chef-cd
aws s3 cp "$VERSION.json.asc" s3://chef-automate-artifacts/dev/latest/automate/manifest.json.asc --acl public-read --profile chef-cd
aws s3 cp "$VERSION.json.sha256sum" s3://chef-automate-artifacts/dev/latest/automate/manifest.json.sha256sum --acl public-read --profile chef-cd

#
# Upload the chef-automate CLI
#

hab pkg install --channel dev chef/automate-cli

tmpdir=$(mktemp -d)
pushd "${tmpdir}"
  zip -j "chef-automate_linux_amd64.zip" "$(hab pkg path chef/automate-cli)/static/linux/chef-automate"

  gpg --armor --digest-algo sha256 --default-key 2940ABA983EF826A --output "chef-automate_linux_amd64.zip.asc" --detach-sign "chef-automate_linux_amd64.zip"
  sha256sum "chef-automate_linux_amd64.zip" > "chef-automate_linux_amd64.zip.sha256sum"
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip" "s3://chef-automate-artifacts/unstable/latest/automate/chef-automate_linux_amd64.zip" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip.asc" "s3://chef-automate-artifacts/unstable/latest/automate/chef-automate_linux_amd64.zip.asc" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip.sha256sum" "s3://chef-automate-artifacts/unstable/latest/automate/chef-automate_linux_amd64.zip.sha256sum" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip" "s3://chef-automate-artifacts/dev/latest/automate/chef-automate_linux_amd64.zip" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip.asc" "s3://chef-automate-artifacts/dev/latest/automate/chef-automate_linux_amd64.zip.asc" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip.sha256sum" "s3://chef-automate-artifacts/dev/latest/automate/chef-automate_linux_amd64.zip.sha256sum" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip" "s3://chef-automate-artifacts/files/automate/$VERSION/chef-automate_linux_amd64.zip" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip.asc" "s3://chef-automate-artifacts/files/automate/$VERSION/chef-automate_linux_amd64.zip.asc" --acl public-read
  aws --profile chef-cd s3 cp "chef-automate_linux_amd64.zip.sha256sum" "s3://chef-automate-artifacts/files/automate/$VERSION/chef-automate_linux_amd64.zip.sha256sum" --acl public-read
popd

#
# Cleanup
#

rm "$VERSION.json"
rm existing-versions.json
rm existing-releases.json
rm updated-versions.json
rm updated-releases.json
rm -r "${tmpdir}"
