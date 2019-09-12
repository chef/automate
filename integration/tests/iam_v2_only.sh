#!/bin/bash

#shellcheck disable=SC2034
test_name="iam v2 only"
# Note: the inspec profile takes care of deleting any migrated v1 legacy
# policies
test_deploy_inspec_profiles=(a2-iam-v2-only-integration)
# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

do_setup() {
    do_setup_default

    hab_pkg_install core/curl
    hab_pkg_install -b core/jo
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

# The inspec tests also test some applications APIs that require the backend
# for the feature to be enabled -- we do this via config to avoid a race between
# the config having been applied, and the services restarted accordingly, and
# the actual test execution.
do_create_config() {
    do_create_config_default
    #shellcheck disable=SC2154
    cat <<EOF >>"$test_config_path"
# Gateway service configuration.
[gateway.v1.sys.service]
enable_apps_feature = true
# event-service configuration.
[event_service.v1.sys.service]
enable_nats_feature = true
# event-gateway
[event_gateway.v1.sys.service]
enable_nats_feature = true
# applications-service
[applications.v1.sys.service]
enable_nats_feature = true
EOF
}

# Note: the inspec tests assert that a viewer or operator user is able
# to retrieve the license status -- so we just add one.
do_deploy() {
    do_deploy_default
    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"
}

do_test_deploy() {
    log_info "run chef-automate iam upgrade-to-v2"
    chef-automate iam upgrade-to-v2 || return 1

    # ensure service startup works with IAM v2:
    # - kill authz-service to force startup,
    # - wait for service status to be healthy again
    log_info "restarting authz-service, waiting 5s for it to come up again"
    pkill -f authz-service && sleep 5 && chef-automate status -w || return 1

    log_info "creating test users with automate-cli"
    chef-automate dev create-iam-dev-users || return 1
    do_test_deploy_default
}
