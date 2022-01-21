#!/bin/bash

#shellcheck disable=SC2034
test_name="proxy"
test_proxy="true"

do_deploy() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --product automate \
        --product builder \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa

    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"

    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts

    timestamp=$(date +"%m-%d-%y-%H-%M")

    log_info "generating admin token"
    if ! token=$(chef-automate iam token create "$timestamp-tok" --admin); then
        log_error "Non-zero exit code, output:"
        log_error "$token"
        return 1
    fi

    export CYPRESS_ADMIN_TOKEN=$token
    export CYPRESS_RUN_FLAKY=$FLAKY

    log_info "fixing dns resolution for '${CONTAINER_HOSTNAME}'"
    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts
    # shellcheck disable=SC2154
    proxyurl="http://${test_proxy_container_name}:3128"
    echo "export http_proxy=${proxyurl}" >> ~/.bashrc
    echo "export https_proxy=${proxyurl}" >> ~/.bashrc
    #shellcheck disable=SC1090
    source ~/.bashrc
}

do_test_deploy() {
    #shellcheck disable=SC2154
    #shellcheck source=integration/helpers/bldr_tests.sh
    source "${source_dir}/helpers/bldr_tests.sh"

    do_test_deploy_default

    log_info "running cypress in e2e"
    cd "${A2_ROOT_DIR}/e2e" || return 1
    export CYPRESS_SKIP_SSO=true
    export CYPRESS_BASE_URL="https://$CONTAINER_HOSTNAME"

    npm install # get dependencies defined in e2e/package.json
    if ! npm run cypress:run:proxy; then
        buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
        return 1
    fi
}
