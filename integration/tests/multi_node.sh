#!/bin/bash

#shellcheck disable=SC2034
test_name="multi_node"
test_external_services=(ha_backend automate_cluster)
#test_backup_restore=true

do_deploy() {
    :
    #chef-automate deploy config.toml \
        #--hartifacts "$test_hartifacts_path" \
        #--override-origin "$HAB_ORIGIN" \
        #--manifest-dir "$test_manifest_path" \
        #--enable-chef-server \
        #--admin-password chefautomate \
        #--accept-terms-and-mlsa
}

do_create_config() {
    :
    #do_create_config_default
    #cat /services/ha_backend.toml >> $test_config_path
}
