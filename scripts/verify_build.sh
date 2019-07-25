#!/usr/bin/env bash
#
# verify_build: Build all hartifacts and other assets required for
# integration testing. The artifacts are uploaded to buildkite
#
set -eo pipefail

if [[ "$CI" != "true" ]]; then
    echo "WARNING: This script is intended to run in buildkite.  It will likely fail outside of buildkite."
fi

log_section_start() {
    echo "--- [$(date -u)] $*"
}

export HAB_NONINTERACTIVE=true
export HAB_STUDIO_SECRET_HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE="accept-no-persist"

RESOLVED_RESULTS_DIR=$(realpath results/)

log_section_start "install ruby"
sudo -E hab pkg install core/ruby
export PATH
PATH="$(hab pkg path core/ruby)/bin:$PATH"
sudo -E "$(hab pkg path core/ruby)"/bin/gem install toml

log_section_start "generate ephemeral origin key"
HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR hab origin key generate chef

log_section_start "download manifests"
curl "https://packages.chef.io/manifests/dev/automate/latest.json" > results/dev.json
curl "https://packages.chef.io/manifests/current/automate/latest.json" > results/current.json
curl "https://packages.chef.io/manifests/acceptance/automate/latest.json" > results/acceptance.json

log_section_start "determine changed components"
mapfile -t changed_components < <(./scripts/changed_components.rb)
if [[ ${#changed_components[@]} -ne 0 ]]; then
    buildkite-agent annotate --style "info" << EOF
This change rebuilds the following components:
$(printf '* %s\n' "${changed_components[@]}")
EOF
else
    buildkite-agent annotate --style "info" "This change rebuilds no components."
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
    component_build="echo \"--- [\$(date -u)] build $component\"; build $component"
    build_commands="${build_commands} $component_build;"
done

if [[ "$build_commands" != "" ]]; then
    # We override HAB_CACHE_KEY_PATH to ensure we only see the key we
    # generated in this build
    HAB_DOCKER_OPTS="--tty" HAB_ORIGIN="" HAB_CACHE_KEY_PATH=$RESOLVED_RESULTS_DIR DO_CHECK=true hab studio -D run "source .studiorc; set -e; $build_commands"
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
log_section_start "create manifest"
.expeditor/create-manifest.rb
mv manifest.json results/build.json

log_section_start "create buildkite artifact"
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
