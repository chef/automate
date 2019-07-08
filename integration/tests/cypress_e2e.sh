test_name=":cypress: e2e tests"
# Note: we only run cypress here, ignore diagnostics and inspec
test_deploy_inspec_profiles=()
test_skip_diagnostics=true

do_test_deploy() {
    log_info "run chef-automate iam upgrade-to-v2"
    if ! output=$(chef-automate iam upgrade-to-v2); then
        log_error "Non-zero exit code, output:"
        log_error "$output"
        return 1
    fi

    log_info "running cypress"
    export CYPRESS_SKIP_SSO=true
    export CYPRESS_BASE_URL="https://localhost"
    cypress run
}
