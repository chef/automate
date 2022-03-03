#!/bin/bash

#shellcheck disable=SC2034
test_name="airgap_ow_upgrade"
test_upgrades=true
test_diagnostics_filters="~purge"

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

    set_version_file

    log_info "Creating update airgap bundle"
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
    chef-automate deploy config.toml \
        --airgap-bundle bundle.aib \
        --admin-password chefautomate \
        --accept-terms-and-mlsa

    # one by-product of these tests is a custom v1 policy
    # it should be preserved by the force-uprade
    run_inspec_tests "${A2_ROOT_DIR}" "a2-iam-v1-integration"
}

do_prepare_upgrade() {
    do_backup_default
    do_prepare_restore_default
}

do_upgrade() {
    #shellcheck disable=SC2154
    chef-automate backup restore --debug \
        --airgap-bundle update.aib \
        --no-check-version \
        "$test_backup_id"

    # verifies IAM force-upgrade was applied,
    # v1 default and custom policies were migrated,
    # and the upgraded system demonstrates expected behavior around IAM permissions
    run_inspec_tests "${A2_ROOT_DIR}" "a2-iam-legacy-integration"
}
