#!/bin/bash

#shellcheck disable=SC2034
test_name="ocid"

#shellcheck source=.studio/chef-server-collection
source .studio/chef-server-collection

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --enable-chef-server \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
        
    do_apply_license    
}

do_test_deploy() {
    test_if_env_vars_are_configured
    test_if_webui_key_is_patched
    test_if_login_working_with_correct_credentials
    test_if_login_failing_with_incorrect_credentials
}
