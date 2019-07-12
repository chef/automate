#!/usr/bin/env bash

run_diagnostics_pre_upgrade() {
    local loadbalancer_url="$1"
    local filters="$2"
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters --lb-url "$loadbalancer_url" --skip-cleanup
}

run_diagnostics_pre_deep_upgrade() {
    # Our CI scripts run diagnostics using the latest `chef-automate` executable.
    # Not all current tests will run correctly against an old A2, so we tag unsafe tests
    # and run the rest.
    local loadbalancer_url="$1"
    local filters="$2"
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters --lb-url "$loadbalancer_url" --skip-cleanup
}

run_diagnostics_post_upgrade() {
    local loadbalancer_url="$1"
    local filters="$2"
    # Verify the old data made it
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters --lb-url "$loadbalancer_url" --skip-generate

    # Make sure we can exercise the entirety of the tests post upgrade
    chef-automate diagnostics run --lb-url "$loadbalancer_url" --skip-cleanup
}

run_inspec_tests() {
    local root_dir=$1
    local test_names=( "${@:2}" )

    for test_name in "${test_names[@]}"
    do
        log_info "Running inspec profile ${test_name}"
        CHEF_LICENSE="accept-no-persist" inspec exec --no-backend-cache "${root_dir}/inspec/${test_name}" || errcode=$? && true;

        if [[ $errcode -ne 0 && $errcode -ne 101  ]]
        then
            log_error "Failed to run inspec profile"
            return 1
        fi
    done
}

verify_packages() {
    log_info "Verifying all Habitat Packages"e
    chef-automate dev verify-packages
    log_info "Done verifying all Habitat Packages"
}
