#!/bin/bash

test_name="chef-server-only"

source .studio/chef-server-collection

# This test
# 1. deploys chef server
# 2. converges the chef client and grabs the ohai_time
# 3. takes a backup, converge the chef-client, and grabs the ohai_time again
# 4. checks that the ohai_time in 2 and the ohai_time in 3 are different
# 5. restores from the backup
# 6. verifies the ohai_time matches the one in 2

do_create_config() {
    do_create_config_default

    cat << EOF >> "$test_config_path"
[erchef.v1.sys.data_collector]
enabled = false
EOF
}

do_deploy() {
    chef-automate deploy config.toml \
        --product chef-server \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_test_deploy() {
    PATH="/hab/bin:/bin" chef-server-ctl test
    test_chef_server_ctl
}
