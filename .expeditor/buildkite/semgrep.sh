#!/bin/bash
set -eou pipefail

# usage: semgrep.sh $BUILDKITE_BRANCH $BUILDKITE_BUILD_URL $BUILDKITE_PULL_REQUEST_BASE_BRANCH

echo "running in $(pwd)"

# Env vars needed by semgrep-agent
export SEMGREP_BRANCH=$1
export SEMGREP_REPO_NAME=chef/automate
# Activates links to buildkite builds in slack notification
export SEMGREP_JOB_URL=$2
# Activates links to git commits in slack notification
export SEMGREP_REPO_URL=https://github.com/chef/automate

BASELINE=$3
MERGE_BASE=$(git merge-base "${BASELINE:-master}" HEAD)
if [ "$SEMGREP_BRANCH" == "master" ] ; then
  echo "build on master; using 'master~1' as base branch" && BASELINE=master~1
elif [ "$SEMGREP_BRANCH" != "master" ] && [ -n "$BASELINE" ] ; then
  echo "PR build on '$SEMGREP_BRANCH' branch; base is $MERGE_BASE (merge-base of '$BASELINE')" && BASELINE=$MERGE_BASE
elif [ "$SEMGREP_BRANCH" != "master" ] && [ -z "$BASELINE" ] ; then
  echo "manual build on '$SEMGREP_BRANCH' branch; using merge-base of master as base ($MERGE_BASE)" && BASELINE=$MERGE_BASE
fi
python -m semgrep_agent --publish-token "$SEMGREP_TOKEN" --publish-deployment "$SEMGREP_ID" --baseline-ref "$BASELINE"
