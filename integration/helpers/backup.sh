#!/bin/bash

assert_backup_missing() {
    log_info "checking to make sure backup $1 doesn't exist"
    chef-automate backup list | \
        awk -v s="$1" '$0~s{r=1} 1; END{exit(r)}' || return 1
}

delete_backup_and_assert_idempotent() {
    # Delete the backup
    #shellcheck disable=SC2154
    log_info "deleting backup ${test_backup_id}"
    chef-automate backup delete -t 300 --yes "${test_backup_id}" || return 1
    # Make sure that our backup is not in the list
    log_info "checking to make sure backup doesn't exist"
    assert_backup_missing "${test_backup_id}"
    # Make sure it's idempotent
    log_info "deleting non-existent backup"
    chef-automate backup delete --yes "${test_backup_id}"
}

setup_broken_backups() {
    cp -r "$A2_ROOT_DIR/integration/fixtures/broken_backups/"* "/var/opt/chef-automate/backups/"
    chown -R hab:hab /var/opt/chef-automate/backups/
}

test_delete_broken_backups() {
    setup_broken_backups
    chef-automate backup delete --yes 20190122155738
    chef-automate backup delete --yes 20190122155739
    assert_backup_missing 20190122155738
    assert_backup_missing 20190122155739
}

test_incorrect_restore_checksums_fail() {
    log_info "Checking that restore fails with incorrect sha256 given..."
    chef-automate backup restore "$test_backup_id" --yes --skip-preflight --sha256 0000000000000000000000000000000000000000000000000000000000000000
    if [ "$?" -ne "69" ]; then
      log_error "fail: 'chef-automate backup restore' with incorrect sha256 succeeded when it should not have"
      false
    else
      log_info "pass"
      true
    fi
}

test_metadata_sha256_mismatch_fails() {
    cp -p "/var/opt/chef-automate/backups/$test_backup_id/deployment-service/metadata.json" "/var/opt/chef-automate/backups/$test_backup_id/deployment-service/metadata.json.original"
    echo "" >> "/var/opt/chef-automate/backups/$test_backup_id/deployment-service/metadata.json"

    log_info "Checking that restore fails when a service backup has a modified metadata.json file"

    #shellcheck disable=SC2154
    chef-automate backup restore "$test_backup_id" --yes --skip-preflight --sha256 "$backup_sha256"
    if [ "$?" -ne "69" ]; then
      log_error "fail: 'chef-automate backup restore' with incorrect sha256 succeeded when it should not have"
      false
    else
      log_info "pass"
      mv "/var/opt/chef-automate/backups/$test_backup_id/deployment-service/metadata.json.original" "/var/opt/chef-automate/backups/$test_backup_id/deployment-service/metadata.json"
      true
    fi
}

test_missing_checksums_file_fails() {
    mv "/var/opt/chef-automate/backups/$test_backup_id/checksums.json" "/var/opt/chef-automate/backups/$test_backup_id/checksums.json.original"

    log_info "Checking that restore fails when a service backup has a missing checksums.json file"
    chef-automate backup restore "$test_backup_id" --yes --skip-preflight --sha256 "$backup_sha256"
    if [ "$?" -ne "69" ]; then
      log_error "fail: 'chef-automate backup restore' with incorrect sha256 succeeded when it should not have"
      false
    else
      log_info "pass"
      mv "/var/opt/chef-automate/backups/$test_backup_id/checksums.json.original" "/var/opt/chef-automate/backups/$test_backup_id/checksums.json"
      true
    fi
}

test_can_regenerate_cert_after_restore() {
    log_info "Checking that automate can regnerate certs after restore"
    # TODO(ssd) 2019-07-12: We stop the deployment service first to
    # work around a bug. The bug is related to the fact that the
    # periodic cert check will notice the deleted cert, regenerate it,
    # and restart us in the middle of the shutdown sequence, leading
    # to problems.
    log_info " - Stopping deployment-service"
    systemctl stop chef-automate
    log_info " - Removing deployment-service certificate"
    rm /hab/svc/deployment-service/data/deployment-service.crt
    log_info " - Waiting for deployment-service to restart"
    systemctl start chef-automate
    wait_for_healthy
}
