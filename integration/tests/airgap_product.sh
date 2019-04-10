test_name="airgap_product"
test_deploy_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

do_build() {
    do_build_default
    set_test_manifest "build.json"
    log_info "Installing harts"
    # We need to make sure the harts are installed so that the bundle creation works
    if ls ${test_hartifacts_path}/*.hart
    then
        hab pkg install ${test_hartifacts_path}/*.hart
    fi

    log_info "Creating airgap bundle"
    chef-automate airgap bundle create \
        --manifest "${test_manifest_path}" \
        --hartifacts "${test_hartifacts_path}" \
        --override-origin "$HAB_ORIGIN" \
        bundle.aib

    # Installation of the artifact should create /hab
    rm -rf /hab
}

do_deploy() {
    chef-automate deploy config.toml \
        --airgap-bundle bundle.aib \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
}

do_test_deploy() {
    do_test_deploy_default
    chef-automate dev verify-packages
}
