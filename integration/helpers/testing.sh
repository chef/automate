#!/usr/bin/env bash

run_diagnostics_pre_upgrade() {
    local loadbalancer_url="$1"
    local filters="$2"
    local pre_upgrade_filters="$3"
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters $pre_upgrade_filters --lb-url "$loadbalancer_url" --skip-cleanup
}

run_diagnostics_post_upgrade() {
    local loadbalancer_url="$1"
    local filters="$2"
    local pre_upgrade_filters="$3"
    # Verify the old data made it
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters $pre_upgrade_filters --lb-url "$loadbalancer_url" --skip-generate

    # Make sure we can exercise the entirety of the tests post upgrade
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters --lb-url "$loadbalancer_url" --skip-cleanup
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
