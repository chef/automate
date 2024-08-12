#!/bin/bash

#shellcheck disable=SC2034
test_name="product-dev"
test_deploy_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

do_deploy() {
    log_info "*** Deploy begins ***"
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --product automate-dev \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa \
        --debug

    log_info "*** Deploy ends ***"

    log_info "*** Applying license ***"


    do_apply_license  

    log_info "*** Done applying license ***"
}
