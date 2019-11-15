#!/bin/bash

#shellcheck disable=SC2034
test_name="bldr_smoke"

test_deploy_inspec_profiles=()
test_skip_diagnostics=true
test_backup_restore=true

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
}

do_test_deploy() {
    #shellcheck disable=SC2154
    #shellcheck source=integration/helpers/bldr_tests.sh
    source "${source_dir}/helpers/bldr_tests.sh"

    bldr_smoke_test
}

do_test_restore() {
    hab pkg install core/hab
    artifacts_dir=$(mktemp -d --suffix="bldr_smoke")
    # TODO(jaym): remove when we upgrade hab to a version
    # that supports hab pkg download
    SSL_CERT_FILE="/hab/svc/automate-load-balancer/data/$CONTAINER_HOSTNAME.cert" \
        hab pkg exec core/hab \
        hab pkg download \
        -u "https://$CONTAINER_HOSTNAME/bldr/v1" \
        -c "unstable" \
        --download-directory "$artifacts_dir" \
        "${TEST_ORIGIN}/${TEST_PKG}"

    rm -r "$artifacts_dir"
}
