#!/bin/bash

#shellcheck disable=SC2034
test_name="iam v2"
# Note: we can't run diagnostics AND inspec, so no inspec here
#shellcheck disable=SC2034
test_diagnostics_filters="~iam-v1"
test_upgrades=true

do_test_deploy() {
    log_info "creating test users with automate-cli"
    chef-automate dev create-iam-dev-users || return 1
    do_test_deploy_default
}
