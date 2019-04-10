#!/bin/bash

set -eou pipefail

branch="expeditor/bump-inspec"
git checkout -b "$branch"

sed -i -E "s#chef/inspec/[0-9\.]+/[0-9]{14}#chef/inspec/$EXPEDITOR_PKG_VERSION/$EXPEDITOR_PKG_RELEASE#" components/compliance-service/habitat/plan.sh
echo "$EXPEDITOR_PKG_VERSION" > INSPEC_VERSION

git add components/compliance-service/habitat/plan.sh INSPEC_VERSION
git commit --message "Bump inspec to $EXPEDITOR_PKG_VERSION/$EXPEDITOR_PKG_RELEASE"  --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

open_pull_request

git checkout -
git branch -D "$branch"
