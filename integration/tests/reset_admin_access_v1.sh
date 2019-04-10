#shellcheck disable=SC2034
#shellcheck disable=SC2039
#shellcheck disable=SC2154
#shellcheck disable=SC1091

test_name="reset admin access v1"
test_deploy_inspec_profiles=(a2-api-integration)
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
    hab pkg install -b core/jq-static

    umask $previous_umask
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

remove_admin_from_admins_team() {
    local token=$(chef-automate admin-token)
    local admins_team_id=$(hab_curl -k -s -H "api-token: $token" https://localhost/api/v0/auth/teams | \
      jq -r '.teams[] | select(.name == "admins").id')
    local admin_user_id=$(hab_curl -k -s -H "api-token: $token" https://localhost/api/v0/auth/users | \
      jq -r '.users[] | select(.username == "admin").id')
    hab_curl -k -s -H "api-token: $token" "https://localhost/api/v0/auth/teams/${admins_team_id}/users" \
      -XPUT -d "{\"user_ids\": [\"$admin_user_id\"]}"
}

# This is what we reset the password to
export AUTOMATE_API_DEFAULT_PASSWORD="reset-admin-password"

do_test_deploy() {
    log_info "Removing admin from the admins team"
    remove_admin_from_admins_team # just an example of how to screw A2 IAM up
    echo -e "\n\n"

    log_info "Restoring default admin access"
    chef-automate iam admin-access restore $AUTOMATE_API_DEFAULT_PASSWORD
    echo -e "\n\n"

    do_test_deploy_default
}
