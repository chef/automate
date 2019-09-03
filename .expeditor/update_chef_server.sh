#!/bin/bash
#
# bump_chef_server.sh: Update various Habitat pins with the packages
# from the latest build in the given channel.
#
# Usage:
#
#    bump_chef_server.sh CHANNEL
#
set -eo pipefail

CHANNEL=${1:-stable}
NO_GIT=${NO_GIT:-false}

if [[ "$NO_GIT" != "true" ]]; then
    branch="expeditor/bump-chef-server"
    git checkout -b "$branch"
fi

declare -A hab_packages
declare -A file_for_pkg

manifest_for_channel() {
    local channel="$1"
    local url="https://packages.chef.io/manifests/$channel/chef-server/latest.json"
    curl "$url"
}

echo "Retrieving and parsing chef-server release manifest from $CHANNEL"
manifest_content="$(manifest_for_channel "$CHANNEL")"
build="$(jq -rn --argjson manifest "$manifest_content" '$manifest.build')"
echo "Found Chef Server $build"
while read -r line; do
    pkg_name=$(echo "$line" | cut -d/ -f2)
    hab_packages[$pkg_name]=$line
done < <(jq -rn --argjson manifest "$manifest_content" '$manifest.packages[]')

file_for_pkg=(
    [chef-server-ctl]="components/automate-cs-nginx/habitat/plan.sh"
    [chef-server-nginx]="components/automate-cs-nginx/habitat/plan.sh"
    [bookshelf]="components/automate-cs-bookshelf/habitat/plan.sh"
    [oc_bifrost]="components/automate-cs-oc-bifrost/habitat/plan.sh"
    [oc_erchef]="components/automate-cs-oc-erchef/habitat/plan.sh"
    [openresty-noroot]="components/automate-workflow-nginx/habitat/plan.sh"
)

for i in "${!file_for_pkg[@]}"; do
    package_name="$i"
    new_ident="${hab_packages[$i]}"
    file_to_update="${file_for_pkg[$i]}"

    echo "Updating pin for $package_name in $file_to_update pins to $new_ident"
    sed -i -r "s|$package_name/[0-9]+(\\.[0-9]+){2,3}/[0-9]{14}|${new_ident#chef/}|" "$file_to_update"

    if [[ "$package_name" != "openresty-noroot" ]]; then
        echo "Updating pkg_version in $file_to_update pins to $build"
        sed -i -r "s|pkg_version=\".*\"|pkg_version=\"$build\"|" "$file_to_update"
    fi
done

if [[ "$NO_GIT" != "true" ]]; then
    git add --all

    git commit --message "Bump Chef Server package versions" \
        --message "This pull request was triggered automatically via Expeditor." \
        --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

    open_pull_request

    # Get back to master and cleanup the leftovers - any changed files
    # left over at the end of this script will get committed to
    # master.
    git checkout -
    git clean -fxd
    git branch -D "$branch"
fi
