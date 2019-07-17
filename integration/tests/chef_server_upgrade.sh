#!/bin/bash

test_name="chef_server_upgrade"
test_upgrades=true
test_upgrade_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

source .studio/chef-server-collection

do_deploy() {
    chef-automate deploy config.toml \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --enable-chef-server \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_test_deploy() {
    PATH="/hab/bin:/bin" chef-server-ctl test
    test_chef_server_ctl
    test_knife
    do_test_deploy_default
}

do_test_upgrade() {
    PATH="/hab/bin:/bin" chef-server-ctl test
    test_chef_server_ctl
    test_knife
    do_test_upgrade_default
}
