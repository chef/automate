#!/bin/bash

#shellcheck disable=SC2034
test_name="deep_upgrade"
test_upgrades=true
test_upgrade_strategy="at-once"
test_loadbalancer_url="https://localhost:4443"

CURRENT_OLDEST_VERSION=20180519154949
OLD_MANIFEST_DIR=components/automate-deployment/testdata/old_manifests/
DEEP_UPGRADE_PATH="${OLD_MANIFEST_DIR}/${CURRENT_OLDEST_VERSION}.json"

do_create_config() {
    do_create_config_default
    #shellcheck disable=SC2154
    echo -e "[load_balancer.v1.sys.service]\nhttps_port = 4443" >> "$test_config_path"
}

do_deploy() {
    # specific to this test

    # DOWNGRADE habitat: old versions of deployment service only work with hab 0.54
    # and lower; they call hab commands which no longer exist in latest hab. When
    # not downgraded, the deploy fails with errors like
    #
    #   time="2018-07-09T21:55:33Z"
    #   level=error
    #   msg="unload failed"
    #   error="exit status 1"
    #   output="error: Found argument 'unload' which wasn't expected, or isn't valid in this context\n\nUSAGE:\n    hab sup [SUBCOMMAND]\n\nFor more information try --help\n"
    #   package=chef/deployment-service/0.1.0/20180519001804
    #
    install_hab "0.54.0"

    #shellcheck disable=SC2154
    serve_manifest "$test_manifest_path"

    #shellcheck disable=SC2154
    chef-automate deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa \
        --skip-preflight
}

do_cleanup() {
    kill %1
}
