#!/bin/bash

test_name="backup"
test_backup_restore=true

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
