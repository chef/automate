#!/usr/bin/env bash

# Get current sha, then find the one right after that via the ancestry-path; that is where we start.
current_sha=$(curl https://packages.chef.io/manifests/current/automate/latest_semver.json 2>/dev/null | jq -r '.git_sha');
# date renders as e.g. "2021-01-27T10:16:58+00:00" so we need to urlencode the plus sign
current_date=$(git log -1 --pretty=%cI "$(git rev-list --ancestry-path "$current_sha"..HEAD | tail -1)" | sed -e s/\+/%2B/)

# Get the date of the latest acceptance sha and add one second to it, otherwise it does not appear in the list!
acceptance_sha=$(curl https://packages.chef.io/manifests/acceptance/automate/latest_semver.json 2>/dev/null | jq -r '.git_sha');
acceptance_date_tmp=$(git log -1 --pretty=%cI "$acceptance_sha")
acceptance_date=$(date -d "$acceptance_date_tmp + 1 second" '+%Y-%m-%dT%H:%M:%S%z' | sed -e s/\+/%2B/)

read -r -d '' message <<EOF
<!here> Automate has been promoted from \`dev\` to \`acceptance\` :successful:

List of changes by commit: https://github.com/chef/automate/compare/${current_sha}...${acceptance_sha}
List of changes by PR: https://github.com/chef/automate/pulls?q=is%3Apr+is%3Amerged+sort%3Acreated-asc+closed%3A${current_date}..${acceptance_date}

Next steps:
(1) Review any PRs you have authored in the PR list above and mark with one of the 'acceptance:*' labels in GitHub.
(2) Add an entry for any customer-facing or other impactful PRs you have authored to the release notes: https://github.com/chef/automate/wiki/Pending-Release-Notes.
(3) Perform general acceptance testing: https://a2-acceptance.cd.chef.co/.
EOF

post_slack_message "a2-release-coordinate" "$message"
