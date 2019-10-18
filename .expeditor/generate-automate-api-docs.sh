#!/bin/bash

set -eou pipefail

mkdir -p /go/src/github.com/chef
ln -s /workspace /go/src/github.com/chef/automate

branch="expeditor/generate-automate-api-docs"
git checkout -b "$branch"

pushd /go/src/github.com/chef/automate/components/automate-chef-io
  make sync_swagger_files
popd

if [[ `git status --porcelain` ]]; then
  git add components/automate-chef-io/data/docs/api_chef_automate

  git commit --message "Sync swagger files for Automate docs." --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

  open_pull_request

  git checkout -
  git clean -fxd
  git branch -D "$branch"
else
  echo "No changed files detected, skipping sync PR."
fi
