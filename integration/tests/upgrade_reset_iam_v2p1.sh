#!/bin/bash

#shellcheck disable=SC2034
test_name="upgrade / reset IAM v2.1"
test_deploy_inspec_profiles=(a2-iam-v2p1-only-integration)
# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

do_setup() {
    do_setup_default

    hab_pkg_install core/curl
    hab_pkg_install -b core/jq-static core/jo
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

do_test_deploy() {
    local output

    expect_iam_version "IAM v1.0" || return 1

    local token
    log_info "checking 'chef-automate admin-token'"
    token="$(chef-automate admin-token)"
    if grep -q "Error" <<< "$token"; then
        log_error "expected token, got error:"
        log_error "$token"
        return 1
    fi

    log_info "v1 -> v2.1"
    expect_iam_upgrade_output "Success: Enabled IAM v2.1" \
        "$(chef-automate iam upgrade-to-v2 --skip-policy-migration)" || return 1
    expect_iam_version "IAM v2.1" || return 1

    log_info "v2.1 -> v1"
    chef-automate iam reset-to-v1 || return 1
    expect_iam_version "IAM v1.0" || return 1

    log_info "v1 -> v2.1"
    expect_iam_upgrade_output "Success: Enabled IAM v2.1" \
        "$(chef-automate iam upgrade-to-v2 --skip-policy-migration)" || return 1
    expect_iam_version "IAM v2.1" || return 1

    do_test_deploy_default
}

expect_iam_upgrade_output() {
    local expected_output="${1}"
    local actual_output="${2}"
    if ! grep -q "$expected_output" <<< "$actual_output"; then
        log_error "unexpected output:"
        log_error "expected (substring): ${expected_output}"
        log_error "actual output: ${actual_output}"
        return 1
    fi
}

expect_iam_version() {
    local expected="${1}"
    log_info "checking 'chef-automate iam version'"
    output="$(chef-automate iam version)"
    if ! grep -q "$expected" <<< "$output"; then
        log_error "expected \"${expected}\", got output:"
        log_error "$output"
        return 1
    fi
}
