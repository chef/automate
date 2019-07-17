#!/bin/bash

#shellcheck disable=SC2034
test_name="airgap_backup"
test_backup_restore=true

do_build() {
    do_build_default
    set_test_manifest "build.json"

    #shellcheck disable=SC2154
    build_bundle bundle.aib "${test_manifest_path}" "${test_hartifacts_path}"
    # Installation of the artifact should create /hab
    rm -rf /hab
}

do_deploy() {
    chef-automate deploy config.toml \
        --airgap-bundle bundle.aib \
        --admin-password chefautomate \
        --accept-terms-and-mlsa \
        --debug
}

do_restore() {
    #shellcheck disable=SC2154
    chef-automate backup restore \
        --airgap-bundle bundle.aib \
        --debug \
        --override-origin "$HAB_ORIGIN" \
        --debug \
        "${test_backup_id}"
}
