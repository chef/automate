#!/bin/bash

#shellcheck disable=SC2034
test_name="airgap_upgrade"
test_upgrades=true

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

    set_version_file
    newversion=$(jq -r -c ".build"  "$test_manifest_dir/dev.json")
    jq --arg val $newversion '. + [$val]' $versionsFile > tmp.$$.json && mv tmp.$$.json $versionsFile
    cat $versionsFile

    log_info "Creating initial airgap bundle"
    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "$test_manifest_dir/dev.json" \
        --workspace workspace \
        --versions $versionsFile \
        bundle.aib

    log_info "Creating update airgap bundle"
    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "$test_manifest_dir/build.json" \
        --hartifacts "${test_hartifacts_path}" \
        --override-origin "$HAB_ORIGIN" \
        --workspace workspace \
        --versions $versionsFile \
        update.aib

    # Installation of the artifact should create /hab
    rm -rf /hab
}

do_deploy() {
    chef-automate deploy config.toml \
        --airgap-bundle bundle.aib \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_upgrade() {
    run_upgrade update.aib
}
