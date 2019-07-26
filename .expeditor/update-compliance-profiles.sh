#!/bin/bash

set -eou pipefail

# EXPEDITOR_PKG_TARGET doesn't seem to always be part of the
# workload. If it isn't there, assume it is correct.
case "${EXPEDITOR_PKG_TARGET:-unknown}" in
    "x86_64-linux")
        echo "EXPEDITOR_PKG_TARGET is set to the expected target of x86_64-linux"
        ;;
    "unknown")
        echo "EXPEDITOR_PKG_TARGET is not set. Assuming this workload is for x86_64-linux"
        ;;
    *)
        echo "EXPEDITOR_PKG_TARGET exists but is not our expected target (got: '$EXPEDITOR_PKG_TARGET', expected: 'x86_64-linux'). Exiting"
        exit 0
        ;;
esac

branch="expeditor/bump-compliance-profiles"
git checkout -b "$branch"

sed -i -E "s#chef/automate-compliance-profiles/[0-9\.]+/[0-9]{14}#chef/automate-compliance-profiles/$EXPEDITOR_PKG_VERSION/$EXPEDITOR_PKG_RELEASE#" components/compliance-service/habitat/plan.sh

git add components/compliance-service/habitat/plan.sh
git commit --message "Bump automate-compliance-profiles"  --message "This pull request was triggered automatically via Expeditor." --message "This change falls under the obvious fix policy so no Developer Certificate of Origin (DCO) sign-off is required."

open_pull_request

git checkout -
git branch -D "$branch"
