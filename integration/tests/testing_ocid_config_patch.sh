#!/bin/bash

#shellcheck disable=SC2034
test_name="ocid-config-patch"

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

do_create_config() {
    do_create_config_default
    
    #shellcheck disable=SC2154
    cat <<EOF >> "$test_config_path"
[ocid.v1.sys.ocid.chef_server_config]
    endpoint="https://test-url.com:443"
    superuser="testuser"
    ssl_verify_mode="verify_peer"
[ocid.v1.sys.ocid.oauth_application_config]
    [[ocid.v1.sys.ocid.oauth_application_config.oauth_applications]]
        name = "test-supermarket"
        redirect_uri = "https://sampleurl.com/auth/chef_oauth2/callback"
EOF
}

do_test_deploy() {
    test_if_env_vars_are_configured_after_patch
    test_if_oauthapps_are_patched_correctly
}
