#!/bin/bash
#shellcheck disable=SC2034
#shellcheck disable=SC2154

test_name="backup-no-sha256"
test_backup_restore=true

do_restore() {
    chef-automate backup restore --debug --override-origin "$HAB_ORIGIN" "$test_backup_id"
}

do_test_restore() {
    do_test_restore_default || return 1
    delete_backup_and_assert_idempotent
}
