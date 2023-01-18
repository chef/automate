#!/bin/bash

#shellcheck disable=SC2034
test_name="proxy"
test_proxy="true"

do_deploy() {
    log_info "$test_hartifacts_path"
    log_info "$HAB_ORIGIN"
    log_info "$test_manifest_path"

    ls $test_hartifacts_path
    cat $test_manifest_path

    curl -x $HTTP_PROXY --proxy-user admin:chefautomate -L https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    curl -x $HTTP_PROXY --proxy-user admin:chefautomate -L https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate-latest.aib

    #shellcheck disable=SC2154
    ./chef-automate deploy config.toml \
        --product automate \
        --product builder \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --admin-password chefautomate \
        --airgap-bundle automate-latest.aib \
        --accept-terms-and-mlsa

    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"

    echo "127.0.0.1 ${CONTAINER_HOSTNAME}" >> /etc/hosts
}

do_test_deploy() {
    #shellcheck disable=SC2154
    #shellcheck source=integration/helpers/bldr_tests.sh
    source "${source_dir}/helpers/bldr_tests.sh"

    do_test_deploy_default
    bldr_smoke_test
}
