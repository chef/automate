#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#shellcheck disable=SC1091

test_name="iam v1 to v2"
test_deploy_inspec_profiles=(a2-iam-v2-integration)
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

    umask $previous_umask
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

do_test_deploy() {
    log_info "Adding v1 policy (that should be migrated)"
    local token
    token=$(chef-automate admin-token)
    hab_curl --fail -s -k -H "api-token: $token" \
      -d "$(jo subjects="$(jo -a user:local:alice team:local:ops)" action=read resource=auth:users)" \
      https://localhost/api/v0/auth/policies

    log_info "run chef-automate iam upgrade-to-v2"
    chef-automate iam upgrade-to-v2 || return 1

    # ensure service startup works with IAM v2:
    # - kill authz-service to force startup,
    # - wait for service status to be healthy again
    pkill -f authz-service && sleep 2 && chef-automate status -w || return 1

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
