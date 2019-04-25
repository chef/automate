#!/usr/bin/env bash

current=$(curl https://packages.chef.io/manifests/current/automate/latest.json 2>/dev/null | jq -r '.git_sha');
acceptance=$(curl https://packages.chef.io/manifests/acceptance/automate/latest.json 2>/dev/null | jq -r '.git_sha');

read -r -d '' message <<EOF
@here :success: A2 has been promoted from \`dev\` to \`acceptance\` :success:

The list of changes can be found here: https://github.com/chef/automate/compare/${current}...${acceptance}

Please take your time to fill out the release notes by *EOD WEDNESDAY*: https://github.com/chef/automate/wiki/Current-Release-Notes
EOF

post_slack_message "a2-team" "$message"
