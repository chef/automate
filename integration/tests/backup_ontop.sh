#!/bin/bash
#shellcheck disable=SC2034
#shellcheck disable=SC2154

test_name="backup-ontop"
test_backup_restore=true


do_prepare_restore() {
    # Run the diagnostics command again. We'll verify that this does not pass
    chef-automate diagnostics run ~remove-this-tag-after-merge --lb-url "$test_loadbalancer_url" --skip-cleanup --save-file "/tmp/context2"
}

do_restore() {
    chef-automate backup restore \
        --debug \
        --override-origin "$HAB_ORIGIN" \
        --yes \
        "$test_backup_id"
}

do_test_restore() {
    do_test_restore_default

    if chef-automate diagnostics run ~remove-this-tag-after-merge --skip-generate --skip-cleanup --save-file "/tmp/context2" >/tmp/context2runlog 2>&1;
    then
        log_error "Expected post-restore diagnostics run to fail when using a context created before the backup was taken."
        log_error "diagnostics run output:"
        cat /tmp/context2runlog
        return 1
    fi
}
