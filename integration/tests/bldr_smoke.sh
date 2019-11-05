#!/bin/bash

#shellcheck disable=SC2034
test_name="bldr_smoke"
# Note: we only run cypress here, ignore diagnostics and inspec
test_deploy_inspec_profiles=()
test_skip_diagnostics=true

do_deploy() {
    do_deploy_default

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
    timestamp=$(date +"%m-%d-%y-%H-%M")

    log_info "fixing dns resolution for '${CONTAINER_HOSTNAME}'"
    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts
}

do_test_deploy() {
    log_info "running cypress in e2e"
    cd "${A2_ROOT_DIR}/e2e" || return 1
    export CYPRESS_SKIP_SSO=true
    export CYPRESS_BASE_URL="https://$CONTAINER_HOSTNAME"
    export CYPRESS_INTEGRATION_FOLDER=cypress/integration/bldr-smoke 
    export CYPRESS_SUPPORT_FILE='false'
    export CYPRESS_TEST_BUILDER='true'
    npm install # get dependencies defined in e2e/package.json
    if ! npm run cypress:run; then
        buildkite-agent artifact upload "cypress/videos/*;cypress/videos/**/*;cypress/screenshots/*;cypress/screenshots/**/*"
        return 1
    fi

    export TEST_ORIGIN="$(cat origin)"
    export TEST_TOKEN="$(cat token)"
    hab origin key generate "$TEST_ORIGIN"
    HAB_ORIGIN=$TEST_ORIGIN hab pkg build "$A2_ROOT_DIR/integration/fixtures/test_plan/"
    SSL_CERT_FILE=/hab/svc/automate-load-balancer/data/$CONTAINER_HOSTNAME.cert hab pkg upload -z "$TEST_TOKEN" -u https://$CONTAINER_HOSTNAME/bldr/v1 $A2_ROOT_DIR/$TEST_ORIGIN*.hart
}
