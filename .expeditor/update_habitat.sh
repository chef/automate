#!/bin/bash

set -eou pipefail

NO_GIT=${NO_GIT:-false}

if [[ "$NO_GIT" != "true" ]]; then
    branch="expeditor/bump-hab"
    git checkout -b "$branch"
fi

HAB_SUP_VERSION=$EXPEDITOR_VERSION
HAB_SUP_RELEASE=$(curl "https://bldr.habitat.sh/v1/depot/channels/core/stable/pkgs/hab-sup/$HAB_SUP_VERSION/latest" | jq -r .ident.release)

# Habitat promotes hab-sup last. We trigger on hab-sup and then query
# the version of the other two packages.
HAB_VERSION=$HAB_SUP_VERSION
HAB_RELEASE=$(curl "https://bldr.habitat.sh/v1/depot/channels/core/stable/pkgs/hab/$HAB_VERSION/latest" | jq -r .ident.release)

HAB_LAUNCH_IDENT_JSON=$(curl "https://bldr.habitat.sh/v1/depot/channels/core/stable/pkgs/hab-launcher/latest" | jq -r .ident)
HAB_LAUNCH_VERSION=$(echo "$HAB_LAUNCH_IDENT_JSON" | jq -r .version)
HAB_LAUNCH_RELEASE=$(echo "$HAB_LAUNCH_IDENT_JSON" | jq -r .release)

sed -i -r "s|\"hab\".*\"version\" =>.*|\"hab\"          => { \"origin\" => \"core\", \"name\" => \"hab\",          \"version\" => \"$HAB_VERSION\", \"release\" => \"$HAB_RELEASE\"},|" .expeditor/create-manifest.rb
sed -i -r "s|\"hab-sup\".*\"version\" =>.*|\"hab-sup\"      => { \"origin\" => \"core\", \"name\" => \"hab-sup\",      \"version\" => \"$HAB_SUP_VERSION\", \"release\" => \"$HAB_SUP_RELEASE\"},|" .expeditor/create-manifest.rb
sed -i -r "s|\"hab-launcher\".*\"version\" =>.*|\"hab-launcher\" => { \"origin\" => \"core\", \"name\" => \"hab-launcher\", \"version\" => \"$HAB_LAUNCH_VERSION\",   \"release\" => \"$HAB_LAUNCH_RELEASE\"}|" .expeditor/create-manifest.rb

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
