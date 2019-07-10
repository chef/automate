#!/usr/bin/env bash
#
# verify_build: Build all hartifacts and other assets required for
# integration testing. The artifacts are uploaded to buildkite
#
set -eo pipefail

if [[ "$CI" != "true" ]]; then
    echo "WARNING: This script is intended to run in buildkite.  It will likely fail outside of buildkite."
fi

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

sudo -E hab pkg install core/ruby
export PATH
PATH="$PATH:$(hab pkg path core/ruby)/bin"
sudo -E "$(hab pkg path core/ruby)"/bin/gem install toml

if [[ $EUID -eq 0 ]]; then
    key_cache="/hab/cache/keys"
else
    key_cache="$HOME/.hab/cache/keys"
fi

keyname=$(hab origin key generate chef | grep -oE 'chef-[0-9]{14}')
cp "${key_cache}/${keyname}.sig.key" results/
cp "${key_cache}/${keyname}.pub" results/

build_secret_key=$(cat "${key_cache}/${keyname}.sig.key")
build_public_key=$(cat "${key_cache}/${keyname}.pub")

echo "Downloading manifests from packages.chef.io"
curl "https://packages.chef.io/manifests/dev/automate/latest.json" > results/dev.json
curl "https://packages.chef.io/manifests/current/automate/latest.json" > results/current.json
curl "https://packages.chef.io/manifests/acceptance/automate/latest.json" > results/acceptance.json

declare -a changed_components

if [[ "$BUILD_ALL" = "true" ]]; then
    for d in components/*/; do
        # Skip the devproxy as you can't build it next to the real automate-ui safely.
        if [[ "$d" == "components/automate-ui-devproxy/" ]]; then
            continue
        fi
        if [[ -f "$d/habitat/plan.sh" ]]; then
            changed_components+=("$d")
        fi
    done

    buildkite-agent annotate --style "info" << EOF
This change rebuilds ALL components because BUILD_ALL=$BUILD_ALL:
$(printf '* %s\n' "${changed_components[@]}")
EOF

else
    mapfile -t changed_components < <(./scripts/changed_components.rb)
    if [[ ${#changed_components[@]} -ne 0 ]]; then
        buildkite-agent annotate --style "info" << EOF
This change rebuilds the following components:
$(printf '* %s\n' "${changed_components[@]}")
EOF
    else
        buildkite-agent annotate --style "info" "This change rebuilds no components."
    fi
fi

mapfile -t modified_sql_files < <(git diff --name-status "$(./scripts/git_difference_expression.rb)" | awk '/^[RMD][0-9]*.*\.sql/{ print $2 }')
if [[ ${#modified_sql_files[@]} -ne 0 ]]; then
    buildkite-agent annotate --append --style "warning" << EOF
This change modifies the following SQL files:
$(printf '* %s\n' "${modified_sql_files[@]}")
EOF
fi

# Build all habitat packages that have changed
build_commands=""
for component in "${changed_components[@]}"; do
    build_commands="${build_commands} build $component;"
done

if [[ "$build_commands" != "" ]]; then
    # We inject the previously created chef origin key into the build.
    # We can't rely on habitat importing it for us because separate
    # builds might be happening on the same machine with a shared key
    # cache.
    build_commands="echo '$build_secret_key' | hab origin key import; ${build_commands}"
    build_commands="echo '$build_public_key' | hab origin key import; ${build_commands}"
    HAB_DOCKER_OPTS="--tty" HAB_ORIGIN="" HAB_CACHE_KEY_PATH=$(realpath results/) DO_CHECK=true hab studio -D run "source .studiorc; set -e; $build_commands"
fi

# Generate a local A2 manifest. This manifest represents the total
# set of packages under test.
#
# We build this manifest after the component build so it can pick up
# newly created packages. This opens us up to a race condition where
# our manifest has newer releases than what we've built. However, this
# should only matter in the upgrade tests which should now use the
# dev.json manifest downloaded before the build as their starting
# point. We are still open to clock-sync affecting package versions
# being older or newer than we might expect.
.expeditor/create-manifest.rb
mv manifest.json results/build.json

# The integration test framework uses this file to decide whether or
# not it needs to build packages directly.
touch results/.prebuilt_artifacts

# The public keys that hab is writing are not readable by the non
# root user since hab 0.77
# TODO(jaym): figure out if this is a problem
sudo chmod a+r results/*.pub

# Upload the build artifacts so they can be shared with other build
# jobs.
tar cvzf results.tar.gz results/
buildkite-agent artifact upload results.tar.gz
