#!/bin/bash

set -eou pipefail

NO_GIT=${NO_GIT:-false}
if [[ "$NO_GIT" != "true" ]]; then
    branch="expeditor/bump-hab"
    git checkout -b "$branch"
fi

# Get latest version by retrieving and parsing the manifest for the
# target version.
TARGET_VERSION=${TARGET_VERSION:-"latest"}
MANIFEST_URL="http://packages.chef.io/files/stable/habitat/$TARGET_VERSION/manifest.json"

MANIFEST=$(curl --silent "$MANIFEST_URL")
PACKAGES=$(jq -r '.packages | ."x86_64-linux"[]' <<< "$MANIFEST")

HAB_SUP_VERSION=$(awk -F/ '/core\/hab-sup\// { print $3 }' <<< "$PACKAGES")
HAB_SUP_RELEASE=$(awk -F/ '/core\/hab-sup\// { print $4 }' <<< "$PACKAGES")
HAB_VERSION=$(awk -F/ '/core\/hab\// { print $3 }' <<< "$PACKAGES")
HAB_RELEASE=$(awk -F/ '/core\/hab\// { print $4 }' <<< "$PACKAGES")
HAB_LAUNCH_VERSION=$(awk -F/ '/core\/hab-launcher\// { print $3 }' <<< "$PACKAGES")
HAB_LAUNCH_RELEASE=$(awk -F/ '/core\/hab-launcher\// { print $4 }' <<< "$PACKAGES")

# Modify relevant source files with the new version.
sed -i -r "s|pins\[\"hab\"\].*\"version\" =>.*|pins[\"hab\"]          = { \"origin\" => \"core\", \"name\" => \"hab\",          \"version\" => \"$HAB_VERSION\", \"release\" => \"$HAB_RELEASE\"}|" .expeditor/create-manifest.rb
sed -i -r "s|pins\[\"hab-sup\"\].*\"version\" =>.*|pins[\"hab-sup\"]      = { \"origin\" => \"core\", \"name\" => \"hab-sup\",      \"version\" => \"$HAB_SUP_VERSION\", \"release\" => \"$HAB_SUP_RELEASE\"}|" .expeditor/create-manifest.rb
sed -i -r "s|pins\[\"hab-launcher\"\].*\"version\" =>.*|pins[\"hab-launcher\"] = { \"origin\" => \"core\", \"name\" => \"hab-launcher\", \"version\" => \"$HAB_LAUNCH_VERSION\",  \"release\" => \"$HAB_LAUNCH_RELEASE\"}|" .expeditor/create-manifest.rb

sed -i -r "s|core/hab/[0-9]+\\.[0-9]+\\.[0-9]+/[0-9]{14}|core/hab/$HAB_VERSION/$HAB_RELEASE|" components/automate-deployment/habitat/plan.sh
sed -i -r "s|RECOMMENDED_HAB_VERSION=\".*\"|RECOMMENDED_HAB_VERSION=\"$HAB_VERSION\"|" .studiorc

if [[ "$NO_GIT" != "true" ]]; then
    git add .expeditor/create-manifest.rb
    git add components/automate-deployment/habitat/plan.sh
    git add .studiorc

    git commit --message "Bump Habitat version"  --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

    open_pull_request

    # Get back to master and cleanup the leftovers - any changed files left over at the end of this script will get committed to master.
    git checkout -
    git clean -fxd
    git branch -D "$branch"
fi
