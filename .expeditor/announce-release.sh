#!/bin/bash

set -eou pipefail

# Download the manifest
curl -o manifest.json "https://packages.chef.io/manifests/${EXPEDITOR_TARGET_CHANNEL}/automate/latest.json"
# Extract the build version from the manifest
build_version=$(jq -r -c ".build"  manifest.json)
# Download the release-notes for our specific build
curl -o release-notes.md "https://packages.chef.io/release-notes/automate/${build_version}.md"

topic_title="Automate 2 version $build_version Released!"
topic_body=$(cat <<EOH
We are delighted to announce the availability of version $build_version of Chef Automate 2.

$(cat release-notes.md)

---
## How to Upgrade

By default Chef Automate 2 will [automatically upgrade](https://automate.chef.io/docs/install/#upgrades) to this new version. If you have disabled automatic upgrades you can manually initiate an upgrade by running:

\`\`\`
chef-automate upgrade run
\`\`\`

As always, we welcome your feedback and invite you to contact us directly or share your [feedback online](https://www.chef.io/feedback/). Thanks for using Chef Automate 2!
EOH
)

# Use Expeditor's built in Bash helper to post our message: https://git.io/JvxPm
post_discourse_release_announcement "$topic_title" "$topic_body"

# Cleanup
rm manifest.json
rm release-notes.md
