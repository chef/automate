#!/bin/bash

#shellcheck disable=SC2034
test_name="backup"
test_backup_restore=true
test_diagnostics_filters="~iam-v2"

do_deploy() {
    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"
}

do_restore() {
    test_metadata_sha256_mismatch_fails || return 1
    test_missing_checksums_file_fails || return 1
    do_restore_default
}

do_test_restore() {
    do_test_restore_default || return 1
    delete_backup_and_assert_idempotent
    test_delete_broken_backups
    test_can_regenerate_cert_after_restore
}
