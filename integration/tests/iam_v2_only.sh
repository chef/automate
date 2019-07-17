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

    # We are defaulting to a umask of 077 to test
    # installations on systems that are super locked down.
    # Briefly override that strict default so we can install
    # packages that non-root users can use (like the hab user
    # for health checks or this script).
    local previous_umask
    previous_umask=$(umask)
    umask 022

    hab pkg install core/curl
    hab pkg install -b core/jo

    umask "$previous_umask"
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
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
