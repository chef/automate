#!/bin/bash

#shellcheck disable=SC2034
test_name="iam v1 to v2"
test_deploy_inspec_profiles=(a2-iam-v2-integration)
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

do_test_deploy() {
    log_info "Adding v1 policy (that should be migrated)"
    local token output
    token=$(chef-automate admin-token)
    hab_curl --fail -s -k -H "api-token: $token" \
      -d "$(jo subjects="$(jo -a user:local:alice team:local:ops)" action=read resource=auth:users)" \
      https://localhost/api/v0/auth/policies

    log_info "run chef-automate iam upgrade-to-v2"
    if ! output=$(chef-automate iam upgrade-to-v2); then
        log_error "Non-zero exit code, output:"
        log_error "$output"
        return 1
    else
        # in CI, we don't want any policies to be skipped
        if grep -q "Skipped policies" <<< "$output"; then
            log_error "Expected no skipped policies, output:"
            log_error "$output"
            return 1
        fi
    fi

    # ensure service startup works with IAM v2:
    # - kill authz-service to force startup,
    # - wait for service status to be healthy again
    log_info "restarting authz-service, waiting 5s for it to come up again"
    pkill -f authz-service && sleep 5 && chef-automate status -w || return 1

    log_info "run chef-automate iam token INSPEC_UPGRADE_IAM_V2_3_ADMIN_TOKEN --admin"
    local admin_token
    admin_token=$(chef-automate iam token create INSPEC_UPGRADE_IAM_V2_3_ADMIN_TOKEN --admin)
    if grep -q Error <<< "$admin_token"; then
        log_error "unexpected error: ${admin_token}"
        return 1
    fi
    export INSPEC_UPGRADE_IAM_V2_3_ADMIN_TOKEN=${admin_token}

    do_test_deploy_default
}
