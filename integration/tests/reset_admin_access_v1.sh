#!/bin/bash

#shellcheck disable=SC2034
test_name="reset admin access v1"
test_deploy_inspec_profiles=(a2-api-integration)
# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

do_setup() {
    do_setup_default

    hab_pkg_install core/curl
    hab_pkg_install -b core/jq-static
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

remove_admin_from_admins_team() {
    local token admins_team_id admin_user_id
    token="$(chef-automate admin-token)"
    admins_team_id=$(hab_curl -k -s -H "api-token: $token" https://localhost/apis/iam/v2/teams | \
      jq -r '.teams[] | select(.name == "admins").id')
    admin_user_id=$(hab_curl -k -s -H "api-token: $token" https://localhost/api/v0/auth/users | \
      jq -r '.users[] | select(.username == "admin").id')
    hab_curl -k -s -H "api-token: $token" "https://localhost/apis/iam/v2/teams/${admins_team_id}/users" \
      -XPUT -d "{\"user_ids\": [\"$admin_user_id\"]}"
}

# This is what we reset the password to
export AUTOMATE_API_DEFAULT_PASSWORD="reset-admin-password"

do_test_deploy() {
    log_info "Removing admin from the admins team"
    remove_admin_from_admins_team # just an example of how to screw A2 IAM up

    log_info "Restoring default admin access"

    chef-automate iam admin-access restore $AUTOMATE_API_DEFAULT_PASSWORD

    do_test_deploy_default
}
