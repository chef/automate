#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#shellcheck disable=SC1091

test_name="reset admin access v2"
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

    umask $previous_umask
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

remove_admin_user() {
    local token=$(chef-automate iam token create ADMIN_TEST --admin)
    hab_curl -fsS -k -H "api-token: $token" "https://localhost/api/v0/auth/users/admin" \
      -XDELETE
}

# This is what we reset the password to
export AUTOMATE_API_DEFAULT_PASSWORD="reset-admin-password"

do_test_deploy() {
    log_info "run chef-automate iam upgrade-to-v2"
    chef-automate iam upgrade-to-v2 || return 1

    log_info "Deleting local admin user"
    remove_admin_user || return 1 # just an example of how to screw A2 IAM up
    echo -e "\n\n"

    log_info "Restoring default admin access"
    chef-automate iam admin-access restore $AUTOMATE_API_DEFAULT_PASSWORD || return 1
    echo -e "\n\n"

    do_test_deploy_default
}
