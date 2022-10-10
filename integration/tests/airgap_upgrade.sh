#!/bin/bash

#shellcheck disable=SC2034
test_name="airgap_upgrade"
test_upgrades=true

do_build() {
    do_build_default
    prepare_upgrade_milestone "current" "3.0.49"
    set_test_manifest "build.json"
    log_info "Installing harts"
    # We need to make sure the harts are installed so that the bundle creation works
    #shellcheck disable=SC2154
    if ls "${test_hartifacts_path}"/*.hart
    then
        hab pkg install "${test_hartifacts_path}"/*.hart
    fi

    set_version_file
    #shellcheck disable=SC2154
    newversion=$(jq -r -c ".build"  "$test_manifest_dir/3.0.49.json")
    #shellcheck disable=SC2154
    jq --arg val "$newversion" '. + [$val]' "$versionsFile" > tmp.$$.json && mv tmp.$$.json "$versionsFile"

    log_info "Creating initial airgap bundle"
    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "$test_manifest_dir/3.0.49.json" \
        --workspace workspace \
        --versions-file "$versionsFile" \
        bundle.aib

    log_info "Creating update airgap bundle"
    #shellcheck disable=SC2154
    chef-automate airgap bundle create \
        --manifest "$test_manifest_dir/build.json" \
        --hartifacts "${test_hartifacts_path}" \
        --override-origin "$HAB_ORIGIN" \
        --workspace workspace \
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
}

do_upgrade() {

    log_info "Airgap upgrading..."

    echo "y" | chef-automate upgrade run --major --airgap-bundle "$airgap_artifact_path" --versions-file "$versionsFile"
    # NOTE: This is a hack
    # The hack above was no longer good enough because we have a thing that needs
    # to be updated that isn't a service
    sleep 45

    #shellcheck disable=SC2154
    wait_for_upgrade "$test_detect_broken_cli" "$test_detect_broken_packages"
    wait_for_healthy
    echo 'y
y' | chef-automate upgrade status --versions-file "$versionsFile"
}
