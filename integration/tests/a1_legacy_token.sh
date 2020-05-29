#!/bin/bash

#shellcheck disable=SC2034
test_name="a1_legacy_token"
test_deploy_inspec_profiles=()
test_skip_diagnostics=true
token="myoldbutsupersecurea1token"

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

    umask "$previous_umask"
}

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

do_create_config() {
    do_create_config_default
    #shellcheck disable=SC2154
    cat <<EOF >> "$test_config_path"
[auth_n.v1.sys.service]
a1_data_collector_token = "$token"
EOF
}

do_test_deploy() {
    # we want to ensure that the token can be used to ingest data
    local response_code
    response_code=$(hab_curl -s -k -H "api-token: $token" "https://localhost/data-collector/v0" \
      -d '{}' -o /dev/null -w '%{response_code}')
    if ! grep -q 400 <<< "$response_code"; then
      log_error "unexpected response code \"$response_code\" (expected 400)"
      return 1
    fi
    log_info "got expected response code (400)"
}
