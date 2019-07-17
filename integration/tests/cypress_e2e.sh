#!/bin/bash
test_name=":cypress: e2e tests"
# Note: we only run cypress here, ignore diagnostics and inspec
test_deploy_inspec_profiles=()
test_skip_diagnostics=true

# Note: these tests use the UI, so a valid license needs to be set up.
# Alternatively, the UI tests could also _apply the license_ and thus
# give us some test coverage for the license apply modal etc; but we
# are not there, yet.
do_deploy() {
    do_deploy_default
    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"

    case $IAM in
        v2)
            log_info "run chef-automate iam upgrade-to-v2 --skip-policy-migration"
            if ! output=$(chef-automate iam upgrade-to-v2 --skip-policy-migration); then
                log_error "Non-zero exit code, output:"
                log_error "$output"
                return 1
            fi
            ;;
        v2.1)
            log_info "run chef-automate iam upgrade-to-v2 --skip-policy-migration --beta2.1"
            if ! output=$(chef-automate iam upgrade-to-v2 --skip-policy-migration --beta2.1); then
                log_error "Non-zero exit code, output:"
                log_error "$output"
                return 1
            fi
            ;;
    esac

    log_info "fixing dns resolution for '${CONTAINER_HOSTNAME}'"
    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts
}

do_test_deploy() {
    log_info "running cypress in e2e"
    cd "${A2_ROOT_DIR}/e2e" || return 1
    export CYPRESS_SKIP_SSO=true
    export CYPRESS_BASE_URL="https://$CONTAINER_HOSTNAME"

    npm install # get dependencies defined in e2e/package.json
    if ! npm run cypress:run; then
        buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
        return 1
    fi
}
