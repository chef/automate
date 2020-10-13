#!/bin/bash

set -eou pipefail

branch="expeditor/generate-automate-api-docs"
git checkout -B "$branch"

git submodule update --init --recursive

pushd components/docs-chef-io
  make sync_swagger_files
  make generate_swagger
popd

if [[ $(git status --porcelain) ]]; then
  git add components/docs-chef-io/data/automate/api_chef_automate
  git add components/docs-chef-io/static/automate-api-docs/all-apis.swagger.json

  git commit --message "Sync swagger files for Automate docs." --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

  open_pull_request

  git checkout -
  git clean -fxd
  git branch -D "$branch"
else
  echo "No changed files detected, skipping sync PR."
fi
