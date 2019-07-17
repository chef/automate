#!/bin/bash

#shellcheck disable=SC2034
test_name="iam v2 only (diagnostics)"
# Note: we can't run diagnostics AND inspec, so no inspec here
#shellcheck disable=SC2034
test_upgrades=true
test_diagnostics_filters="~iam-v1"

do_test_deploy() {
    log_info "run chef-automate iam upgrade-to-v2 --skip-policy-migration"
    chef-automate iam upgrade-to-v2 --skip-policy-migration || return 1

    # ensure service startup works with IAM v2:
    # - kill authz-service to force startup,
    # - wait for service status to be healthy again
    log_info "restarting authz-service, waiting 5s for it to come up again"
    pkill -f authz-service && sleep 5 && chef-automate status -w || return 1

    log_info "creating test users with automate-cli"
    chef-automate dev create-iam-dev-users || return 1
    do_test_deploy_default
}
