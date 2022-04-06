#!/bin/bash

#shellcheck disable=SC2034
test_name="ha_data_services_migrate"
test_external_services=(ha_backend)
test_diagnostics_filters="~purge"
test_upgrades=true

CURRENT_OLDEST_VERSION=20190501153509
OLD_MANIFEST_DIR="${A2_ROOT_DIR}/components/automate-deployment/testdata/old_manifests/"
DEEP_UPGRADE_PATH="${OLD_MANIFEST_DIR}/${CURRENT_OLDEST_VERSION}.json"


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

    log_info "Creating initial airgap bundle"
    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "${DEEP_UPGRADE_PATH}" \
        bundle.aib

    log_info "Creating update airgap bundle"

    set_version_file

    # shellcheck disable=SC2154
    download_manifest_version "current" "20220329091442" "$test_manifest_dir/20220329091442.json"
    set_test_manifest "20220329091442.json"
    append_version_file

    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "$test_manifest_dir/build.json" \
        --hartifacts "${test_hartifacts_path}" \
        --override-origin "$HAB_ORIGIN" \
        --versions-file "$versionsFile" \
        update.aib


    # Installation of the artifact should create /hab
    rm -rf /hab
}

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --airgap-bundle bundle.aib \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_prepare_upgrade() {
    do_backup_default
    cp -r /var/opt/chef-automate/backups/automate-elasticsearch-data \
        /services/ha_backend_backups/automate-elasticsearch-data
    chown -R hab:hab /services/ha_backend_backups/automate-elasticsearch-data
    do_prepare_restore_default
}

do_upgrade() {
    #shellcheck disable=SC2154
    chef-automate backup restore --debug \
        --airgap-bundle update.aib \
        --patch-config /services/ha_backend.toml \
        --no-check-version \
        "$test_backup_id"
}

do_test_upgrade() {
    do_test_upgrade_default
    # doing the patch again shouldn't change anything because
    # the restore should have patched the config
    log_info "Making sure patch config doesn't change anything"
    chef-automate config patch /services/ha_backend.toml
    do_test_upgrade_default
}
