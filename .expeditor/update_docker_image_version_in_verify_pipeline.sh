#!/bin/bash

set -eou pipefail

# only bump the sha256 digest for the "latest" tag
if [[ "$EXPEDITOR_TAG" != "latest" ]]; then
  exit 0
fi

branch="expeditor/bump-chefes/buildkite"
git checkout -b "$branch"

sed -i -r "s|image_sha256: .+|image_sha256: ${EXPEDITOR_SHA256_DIGEST#"sha256:"}|" .expeditor/verify.pipeline.yml
sed -i -r "s|image_sha256: .+|image_sha256: ${EXPEDITOR_SHA256_DIGEST#"sha256:"}|" .expeditor/verify_private.pipeline.yml
sed -i -r "s|image_sha256: .+|image_sha256: ${EXPEDITOR_SHA256_DIGEST#"sha256:"}|" .expeditor/nightly.pipeline.yml

git add .expeditor/nightly.pipeline.yml
git add .expeditor/verify.pipeline.yml
git add .expeditor/verify_private.pipeline.yml

# give a friendly message for the commit and make sure it's noted for any future audit of our codebase that no
# DCO sign-off is needed for this sort of PR since it contains no intellectual property
git commit --message "Bump chefes/buildkite version" --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

open_pull_request

# Get back to master and cleanup the leftovers - any changed files left over at the end of this script will get committed to master.
git checkout -
git clean -fxd
git branch -D "$branch"
