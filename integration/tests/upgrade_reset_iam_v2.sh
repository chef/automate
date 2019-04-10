#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#shellcheck disable=SC1091

test_name="upgrade / reset IAM v2"
test_deploy_inspec_profiles=(a2-api-integration)
# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

do_setup() {
    do_setup_default

    # We are defaulting to a umask of 077 to test
    # installations on systems that are super locked down.
    # Briefly override that strict default so we can install
    # packages that non-root users can use (like the hab user
    # for health checks or this script).
    local previous_umask
    previous_umask=$(umask)
    umask 022

    hab pkg install core/curl
    hab pkg install -b core/jq-static core/jo

    umask $previous_umask
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

do_test_deploy() {
    local output

    log_info "checking 'chef-automate iam version'"
    output=$(chef-automate iam version)
    if ! grep -q "IAM v1" <<< "$output"; then
        log_error "expected \"IAM v1\", got output:"
        log_error "$output"
        return 1
    fi

    local token
    log_info "checking 'chef-automate admin-token'"
    token=$(chef-automate admin-token)
    if grep -q "Error" <<< "$token"; then
        log_error "expected token, got error:"
        log_error "$token"
        return 1
    fi

    log_info "Upgrading to IAM v2"
    chef-automate iam upgrade-to-v2 || return 1

    log_info "Checking IAM version"
    output=$(chef-automate iam version)
    if ! grep -q "IAM v2" <<< "$output"; then
        log_error "expected \"IAM v2\", got output:"
        log_error "$output"
        return 1
    fi
    log_info "On $output"

    # Check that the number of policies stored is exactly equal to the number
    # of default policies
    log_info "Verifying V2 policies"
    local policies

    policies=$(curl -s -k -H "api-token: $token" https://localhost/apis/iam/v2beta/policies)
    if ! jq -e '.policies | length == 13' <<< "$policies" >/dev/null; then
        log_error "Found the wrong number of policies, expected 13 policies, got response:"
        log_error "$policies"
        return 1
    fi

    log_info "Verifying 3 teams exist (admins, operators, viewers)"
    local teams
    teams=$(curl -s -k -H "api-token: $token" https://localhost/api/v0/auth/teams)
    if ! jq -e '.teams | length == 3' <<< "$teams" >/dev/null; then
        log_error "Found the wrong number of teams, expected 3 teams, got response:"
        log_error "$teams"
        return 1
    fi

    log_info "Upgrading to IAM v2 successful"

    log_info "Attempting to upgrade to IAM v2 a second time (expect failure)"
    output="$(chef-automate iam upgrade-to-v2 2>&1)" # capture stderr
    local expected_output="You have already upgraded to IAM v2."
    if ! grep -q "$expected_output" <<< "$output"; then
        log_error "unexpected output:"
        log_error "expected (substring): ${expected_output}"
        log_error "actual output: ${output}"
        return 1
    fi

    log_info "Reset to IAM v1"
    chef-automate iam reset-to-v1 || return 1

    log_info "Upgrading to IAM v2 (again, successful)"
    output=$(chef-automate iam upgrade-to-v2)
    local expected_output="Success: Enabled IAM v2"
    if ! grep -q "$expected_output" <<< "$output"; then
        log_error "unexpected output:"
        log_error "expected (substring): ${expected_output}"
        log_error "actual output: ${output}"
        return 1
    fi

    log_info "Reset to IAM v1 (again)"
    chef-automate iam reset-to-v1 || return 1

    output=$(chef-automate iam version)
    if ! grep -q "IAM v1" <<< "$output"; then
        log_error "expected \"IAM v1\", got output:"
        log_error "$output"
        return 1
    fi

    log_info "Reset to IAM v1 successful"

    do_test_deploy_default
}
