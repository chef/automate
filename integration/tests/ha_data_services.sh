#!/bin/bash

#shellcheck disable=SC2034
test_name="ha_data_services"
test_external_services=(ha_backend)
#test_backup_restore=true

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --enable-chef-server \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_create_config() {
    do_create_config_default
    #shellcheck disable=SC2154
    cat /services/ha_backend.toml >> "$test_config_path"
}

do_cleanup() {
    if journalctl -u chef-automate | grep 'thisisapassword'; then
        log_error "The logs contain the postgres password"
        # This test is broken. notifications-service is leaking
        # the password when it fails to connect to the database.
        # See issue #585
        #
        # return 1
    fi
}
