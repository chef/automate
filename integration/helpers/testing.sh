#!/usr/bin/env bash

run_diagnostics_pre_upgrade() {
    local loadbalancer_url="$1"
    local filters="$2"
    local pre_upgrade_filters="$3"
    local opts="$4"
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters $pre_upgrade_filters --lb-url "$loadbalancer_url" --skip-cleanup $opts
}

run_diagnostics_post_upgrade() {
    local loadbalancer_url="$1"
    local filters="$2"
    local pre_upgrade_filters="$3"
    local opts="$4"
    # Verify the old data made it
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters $pre_upgrade_filters --lb-url "$loadbalancer_url" --skip-generate

    # Make sure we can exercise the entirety of the tests post upgrade
    # shellcheck disable=SC2086
    chef-automate diagnostics run $filters --lb-url "$loadbalancer_url" --skip-cleanup $opts
}

run_inspec_tests() {
    local root_dir=$1
    local test_names=( "${@:2}" )

    for test_name in "${test_names[@]}"
    do
        log_info "Running inspec profile ${test_name}"
        HAB_LICENSE="accept-no-persist" CHEF_LICENSE="accept-no-persist" hab pkg exec chef/inspec inspec exec --no-backend-cache "${root_dir}/inspec/${test_name}" || errcode=$? && true;

        if [[ $errcode -ne 0 && $errcode -ne 101  ]]
        then
            log_error "Failed to run inspec profile"
            return 1
        fi
    done
}

verify_packages() {
    log_info "Verifying all Habitat Packages"
    chef-automate dev verify-packages
    log_info "Done verifying all Habitat Packages"
}

no_panic_check() {
    log_info "Checking for Go panics in the logs"
    if journalctl -u chef-automate | grep 'panic: ';then
        log_error "Found possible panic in the logs!"
        log_error "See uploaded logs for details."
        return 1
    fi
}
