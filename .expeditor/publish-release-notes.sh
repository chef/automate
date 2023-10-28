#!/bin/bash

set -eou pipefail

source_channel=$EXPEDITOR_PROMOTABLE

# Download the manifest
aws s3 cp "s3://chef-automate-artifacts/${source_channel}/latest/automate/manifest_semver.json" manifest.json --profile chef-cd

build_version=$(jq -r -c ".version"  manifest.json)

git clone "https://x-access-token:${GITHUB_TOKEN}@github.com/chef/automate.wiki.git"

pushd ./automate.wiki
  # Publish release notes to S3
  aws s3 cp Pending-Release-Notes.md "s3://chef-automate-artifacts/release-notes/automate/${build_version}.md" --acl public-read --content-type "text/plain" --profile chef-cd
  aws s3 cp Pending-Release-Notes.md "s3://chef-automate-artifacts/${EXPEDITOR_TARGET_CHANNEL}/latest/automate/release-notes.md" --acl public-read --content-type "text/plain" --profile chef-cd

  # Reset "Pending Release Notes" wiki page
  cat >./Pending-Release-Notes.md <<EOH

## Upgrade Journey

Chef lets you choose your **upgrade journey** based on your current version of Chef Automate. You can do all the version upgrades manually.

| Your Current Version | Upgrade To |
| -------------------- | ---------- |
| Any version before 20220329091442| 20220329091442|
| 20220329091442| 3.0.x|
| 3.0.49| 4.x|

See the [Chef Automate 4.x upgrade documentation](https://docs.chef.io/automate/major_upgrade_4.x/) for more information. 

## New Features
-

## Improvements
-

## Compliance Profile Updates
-

## Bug Fixes
-

## Maintenance
-

## Backward Incompatibilities
-

## Security

### Security Improvements
(examples: new security configurations)
-
### Security Updates
(examples: dependency updates, CVE fixes)
-

## Chef Packaged Product Versions

This release uses:
- Chef Habitat version:
- Chef Habitat Builder version:
- Chef Infra Server version:
- Chef InSpec version:

## Service Versions

This release uses:
- Postgres:
- OpenSearch:
- Nginx:
- Haproxy:
- Dex:

## Supported External Chef Products

This release supports the following external chef products:
- Chef Infra Server version: 14.0.58+
- Chef Inspec version: 4.3.2+
- Chef Infra Client: 17.0.242+
- Chef Habitat: 0.81+

## Supported Framework Versions

This release is built on the following framework versions:

- GoLang: 1.15
- OpenJDK: 11.0.17+8
- Angular: 11.2.6

View the [package manifest](https://packages.chef.io/manifests/current/automate/latest_semver.json) for the latest release.

EOH

  # Push changes back up to GitHub
  git add .
  git commit -m "Release Notes for promoted build $build_version"
  git push origin master
popd

rm -rf automate.wiki
