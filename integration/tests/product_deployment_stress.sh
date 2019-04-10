test_name="product_deployment_stress"
test_deploy_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

do_deploy() {
    # adds `--enable-deploy-order-stress-mode` to the default version of the
    # command
    chef-automate deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa \
        --enable-deploy-order-stress-mode
}

