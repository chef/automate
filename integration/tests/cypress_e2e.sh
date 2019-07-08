test_name=":cypress: e2e tests"
# Note: we only run cypress here, ignore diagnostics and inspec
test_deploy_inspec_profiles=()
test_skip_diagnostics=true

do_deploy() {
    do_deploy_default
    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"
}

do_test_deploy() {
    log_info "run chef-automate iam upgrade-to-v2"
    if ! output=$(chef-automate iam upgrade-to-v2); then
        log_error "Non-zero exit code, output:"
        log_error "$output"
        return 1
    fi

    log_info "fixing dns resolution for '${CONTAINER_HOSTNAME}'"
    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts

    log_info "running cypress in e2e"
    cd "${A2_ROOT_DIR}/e2e"
    export CYPRESS_SKIP_SSO=true
    export CYPRESS_BASE_URL="https://$CONTAINER_HOSTNAME"
    if ! cypress run; then
        buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
        return 1
    fi
}
