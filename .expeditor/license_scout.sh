#!/bin/bash

set -eou pipefail

# license_scout uses licensee internally. licensee reads OCTOKIT_ACCESS_TOKEN
# from the environment to make authenticated requests to github. This increases
# the API rate limits that github enforces. Our license checks now read so many
# licenses from github that license_scout will fail if we are not authenticated
# cf. https://github.com/licensee/licensee/blob/da21bba0352f8086920ec59e61c8dbd93f4f0e6d/lib/licensee/projects/github_project.rb#L61
OCTOKIT_ACCESS_TOKEN=$GITHUB_TOKEN
export OCTOKIT_ACCESS_TOKEN

echo "--- Installing Chef UI Library dependencies"
pushd components/chef-ui-library
  npm install
  npm run build
popd

echo "--- Installing Automate UI dependencies"
pushd components/automate-ui
  npm install
popd

echo "--- Installing Elixir dependencies"
pushd components/notifications-service/server
  mix local.hex --force
  mix deps.get
popd

echo "--- Installing Ruby dependencies"
pushd components/automate-workflow-ctl/
  bundle install
popd

echo "+++ Running License Scout"
# a bug requires the use of `--format csv` but the
# format of the generated manifest is still json
license_scout --only-show-failures --format csv
