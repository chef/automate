#!/bin/bash
#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#shellcheck disable=SC1091

test_name="chef-server-backup"
test_backup_restore=true

source .studio/chef-server-collection

# This test
# 1. deploys a2 with a chef server
# 2. converges the chef client and grabs the ohai_time
# 3. takes a backup, converge the chef-client, and grabs the ohai_time again
# 4. checks that the ohai_time in 2 and the ohai_time in 3 are different
# 5. restores from the backup
# 6. verifies the ohai_time matches the one in 2

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
    do_test_deploy_default
}

do_backup() {
    # Get ohai_time before backup
    converge_chef_client
    initial_ohai_time=$(ohai_time)

    # Backup
    chef-automate backup create

    # Get the backup id
    test_backup_id=$(chef-automate backup list | tail -1 | awk '{print $1}')

    # Get ohai_time after backup
    converge_chef_client
    updated_ohai_time=$(ohai_time)

    if [[ $initial_ohai_time = "$updated_ohai_time"  ]]
        then
	    log_error "Initial ohai_time ($initial_ohai_time) should not match updated ohai_time ($updated_ohai_time)."
            return 1
    fi
    log_info "Initial ohai_time ($initial_ohai_time) correctly does not match updated ohai_time ($updated_ohai_time)."
}

do_restore() {
   chef-automate backup restore --debug --override-origin "$HAB_ORIGIN" "$test_backup_id"
   restored_ohai_time=$(ohai_time)

   # Check ohai_time after restore matches ohai_time from backup
   if [[ $restored_ohai_time != "$initial_ohai_time"  ]]

       then
	   log_error "Restored ohai_time ($restored_ohai_time) should match ohai_time ($initial_ohai_time) from backup."
           return 1
   fi
   log_info "Restored ohai_time ($restored_ohai_time) correctly matches ohai_time ($initial_ohai_time) from backup."

   delete_backup_and_assert_idempotent
}
