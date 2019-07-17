#!/bin/bash
#shellcheck disable=SC2034
test_name="a1_migration"
test_container_name="a1-migration.test"
test_upgrade_strategy="none"
test_deploy_inspec_profiles=(a2-upgrade-from-v1-integration)

# Note: this matches the hashes in a1stub/server.go,
#       $2a$12$SWA2q.A2Pe8PzNw.i7DcruoIoq.Lgvz0G7O07.V21I077PveSkGy6
#       and
#       $2a$12$DpdJVmcRKtz8LFB3cZ.QrOWA.3XrbW6htKrzKl1xT4Z/XLGgFR90y
export AUTOMATE_API_DEFAULT_PASSWORD="migrated-admin-password"
export A1_BUILDER_PASSWORD="migrated-builder-password"

do_build() {
    do_build_default
    sync_a1_migration_data
}

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate upgrade-from-v1 "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --upgrade-strategy "$test_upgrade_strategy" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --enable-chef-server \
        --enable-workflow \
        --skip-preflight \
        --self-test \
        --debug \
        --yes
}


do_test_deploy() {
    do_test_deploy_default
    chef_server_migration_smoke_tests
    PATH="/hab/bin:/bin" chef-server-ctl test

    workflow_server_migration_smoke_tests
}

workflow_server_migration_smoke_tests() {
    cmds=(
        "workflow-ctl list-enterprises"
        "workflow-ctl list-enterprises | grep test"
    )

    for c in "${cmds[@]}"; do
        local cmd="PATH=/hab/bin:/bin $c"
        log_info "Running '${cmd}'"
        eval "$cmd"
    done
}

chef_server_migration_smoke_tests() {
    cmds=(
        "chef-server-ctl org-list | grep test"
        "chef-server-ctl user-list | grep test-admin"
    )

    for c in "${cmds[@]}"; do
        local cmd="PATH=/hab/bin:/bin $c"
        log_info "Running '${cmd}'"
        eval "$cmd"
    done
}
