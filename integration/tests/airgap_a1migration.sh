#!/bin/bash

#shellcheck disable=SC2034
test_name="airgap_a1_migration"
test_container_name="a1-migration.test"
test_upgrade_strategy="none"
test_deploy_inspec_profiles=(a2-migrate-from-v1-integration)
test_diagnostics_filters="~iam-v2"

# Note: this matches the hashes in a1stub/server.go,
#       $2a$12$SWA2q.A2Pe8PzNw.i7DcruoIoq.Lgvz0G7O07.V21I077PveSkGy6
#       and
#       $2a$12$DpdJVmcRKtz8LFB3cZ.QrOWA.3XrbW6htKrzKl1xT4Z/XLGgFR90y
export AUTOMATE_API_DEFAULT_PASSWORD="migrated-admin-password"
export A1_BUILDER_PASSWORD="migrated-builder-password"


do_build() {
    do_build_default
    set_test_manifest "build.json"
    log_info "Installing harts"
    # We need to make sure the harts are installed so that the bundle creation works
    #shellcheck disable=SC2154
    if ls "${test_hartifacts_path}"/*.hart
    then
        hab pkg install "${test_hartifacts_path}"/*.hart
    fi

    log_info "Creating airgap bundle"
    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "${test_manifest_path}" \
        --hartifacts "${test_hartifacts_path}" \
        --override-origin "$HAB_ORIGIN" \
        bundle.aib

    sync_a1_migration_data

    # Installation of the artifact should create /hab
    rm -rf /hab
}

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate migrate-from-v1 "$test_config_path" \
        --airgap-bundle bundle.aib \
        --admin-password chefautomate \
        --skip-preflight \
        --self-test \
        --debug \
        --yes
}
