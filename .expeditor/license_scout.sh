#!/bin/bash

set -eou pipefail

log_section_start() {
    echo "--- [$(date -u)] $*"
}

log() {
    echo "[$(date -u)] $*"
}

# license_scout uses licensee internally. licensee reads OCTOKIT_ACCESS_TOKEN
# from the environment to make authenticated requests to github. This increases
# the API rate limits that github enforces. Our license checks now read so many
# licenses from github that license_scout will fail if we are not authenticated
# cf. https://github.com/licensee/licensee/blob/da21bba0352f8086920ec59e61c8dbd93f4f0e6d/lib/licensee/projects/github_project.rb#L61
OCTOKIT_ACCESS_TOKEN=$GITHUB_TOKEN
export OCTOKIT_ACCESS_TOKEN

upload_dep_manifest() {
  if command -v buildkite-agent; then
    dep_json='/go/src/github.com/chef/automate/a2-dependency-licenses.json'

    if [ -f ${dep_json} ]; then
      if ! buildkite-agent artifact upload ${dep_json}; then
        echo "Failed to upload ${dep_json}"
      fi
    fi
  fi
}

trap upload_dep_manifest EXIT

log_section_start "Uninstalling License Scout"
gem uninstall license_scout -x
log "Finished Uninstalling License Scout"

log_section_start "Installing License Scout"
gem install license_scout -v 2.5.1
log "Finished Installing License Scout"

log_section_start "Installing Chef UI Library dependencies"
pushd components/chef-ui-library
  log "BEGIN npm install"
  npm install
  log "END npm install"
  log "BEGIN npm run build"
  npm run build
  log "END npm run build"
  popd
log "Finished installing Chef UI Library dependencies"

log_section_start "Installing Automate UI dependencies"
pushd components/automate-ui
  npm install
popd
log "Finished installing Automate UI dependencies"

log_section_start "Installing Elixir dependencies"
pushd components/notifications-service/server
  git config --global url."https://github.com/".insteadOf git://github.com/
  log "git config updated"
  log "Print mix.lock file:"
  cat mix.lock
  log "End of mix.lock file"
  mix local.hex --force
  mix deps.get
popd
log "Finished installing Elixir dependencies"

log_section_start "Installing Ruby dependencies"
pushd components/automate-workflow-ctl/
  bundle install
popd
log "Finished installing Ruby dependencies"

log_section_start "Installing Go dependencies"
go mod download
log "Finished installing Go dependencies"

log_section_start "Running License Scout"
# a bug requires the use of `--format csv` but the
# format of the generated manifest is still json
license_scout --only-show-failures --format csv
log "Done!"
