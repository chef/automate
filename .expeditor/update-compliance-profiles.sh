#!/bin/bash

set -eou pipefail

if [[ "$EXPEDITOR_PKG_TARGET" != "x86_64-linux" ]]; then
    echo "Ignoring $EXPEDITOR_PKG_VERSION/$EXPEDITOR_PKG_RELEASE because its target is '$EXPEDITOR_PKG_TARGET' (expected 'x86_64-linux')"
    exit 0
fi

branch="expeditor/bump-compliance-profiles"
git checkout -b "$branch"

sed -i -E "s#chef/automate-compliance-profiles/[0-9\.]+/[0-9]{14}#chef/automate-compliance-profiles/$EXPEDITOR_PKG_VERSION/$EXPEDITOR_PKG_RELEASE#" components/compliance-service/habitat/plan.sh

git add components/compliance-service/habitat/plan.sh
git commit --message "Bump automate-compliance-profiles"  --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

open_pull_request

git checkout -
git branch -D "$branch"
