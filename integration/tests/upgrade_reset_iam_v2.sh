#!/bin/bash

#shellcheck disable=SC2034
test_name="upgrade / reset IAM v2"
test_deploy_inspec_profiles=(a2-api-integration)
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

    log_info "v1 -> v2"
    expect_iam_upgrade_output "Success: Enabled IAM v2" \
        "$(chef-automate iam upgrade-to-v2)" || return 1
    expect_iam_version "IAM v2.0" || return 1

    # Check that the number of policies stored is exactly equal to the number
    # of default policies
    log_info "Verifying V2 policies"
    local policies

    policies="$(curl -s -k -H "api-token: $token" https://localhost/apis/iam/v2beta/policies)"
    if ! jq -e '.policies | length == 13' <<< "$policies" >/dev/null; then
        log_error "Found the wrong number of policies, expected 13 policies, got response:"
        log_error "$policies"
        return 1
    fi

    log_info "Verifying 3 teams exist (admins, operators, viewers)"
    local teams
    teams="$(curl -s -k -H "api-token: $token" https://localhost/api/v0/auth/teams)"
    if ! jq -e '.teams | length == 3' <<< "$teams" >/dev/null; then
        log_error "Found the wrong number of teams, expected 3 teams, got response:"
        log_error "$teams"
        return 1
    fi

    log_info "v2 -> v2 (expect failure)"
    expect_iam_upgrade_output "You are already on IAM version v2." \
        "$(chef-automate iam upgrade-to-v2 2>&1)" || return 1

    log_info "v2 -> v2.1"
    expect_iam_upgrade_output "Success: Enabled IAM v2.1" \
        "$(chef-automate iam upgrade-to-v2 --beta2.1)" || return 1
    expect_iam_version "IAM v2.1" || return 1

    log_info "v2.1 -> v1"
    chef-automate iam reset-to-v1 || return 1
    expect_iam_version "IAM v1.0" || return 1

    log_info "v1 -> v2.1"
    expect_iam_upgrade_output "Success: Enabled IAM v2.1" \
        "$(chef-automate iam upgrade-to-v2 --beta2.1)" || return 1

    log_info "v2.1 -> v2"
    expect_iam_upgrade_output "Success: Enabled IAM v2" \
        "$(chef-automate iam upgrade-to-v2)" || return 1
    expect_iam_version "IAM v2.0" || return 1

    log_info "v2 -> v1"
    chef-automate iam reset-to-v1 || return 1
    expect_iam_version "IAM v1.0" || return 1

    log_info "v1 -> v1 (expect failure)"
    expect_iam_upgrade_output "You are already on IAM version v1." \
        "$(chef-automate iam reset-to-v1 2>&1)" || return 1

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
