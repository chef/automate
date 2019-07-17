#!/bin/bash
test_name="security"
test_deploy_inspec_profiles=()
test_skip_diagnostics=true

do_deploy() {
    chef-automate deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --enable-chef-server \
        --enable-workflow \
        --accept-terms-and-mlsa \
        --debug
}

do_test_deploy() {
    do_test_deploy_default

    source "${source_dir}/helpers/ssl_tests.sh"
    run_ssl_scan
}
