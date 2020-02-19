#!/bin/bash
# The purpose of this test is to make sure that only those
# who possess the key for a certificate part of the a2 deployment
# can use the certificate to authenticate either through the
# automate-load-balancer or automate-gateway
cert_auth_tests() {
    log_info "Starting the Certificate Authentication test suite"
    hab pkg install core/curl
    #shellcheck disable=SC2154
    log_info "Generating policy to allow ${authorized_service_name} to GET the data collector endpoint"
    grant_permissions
    echo -e "\n\n"
    test_cert_from_different_ca
    echo -e "\n\n"
    test_authorized_cert
    echo -e "\n\n"
    test_authorized_cert_in_header_authenticated
    echo -e "\n\n"
    test_authorized_cert_in_header_unauthenticated
}

# The curl that's installed in the container gives different exit codes
hab_curl() {
    hab pkg exec core/curl curl "$@"
}

grant_permissions() {
    local token
    token="$(date +%s | xargs -I % chef-automate iam token create admin-token-% --admin)"
    hab_curl --insecure -H "api-token: $token" https://localhost/api/v0/auth/policies --data @<(cat <<EOF
    {
        "subjects": [
            "tls:service:$(authorized_service_name):*"
        ],
        "resource": "ingest:status",
        "action": "read"
    }
EOF
)
}

test_cert_from_different_ca() {
    log_info "Running test_cert_from_different_ca"
    cat <<EOF
    This test makes sure that a valid cert that is not signed
    by the A2 deployment CA is rejected

EOF

    # This is annoying. nginx doesn't fail the handshake and gateway does
    echo "...Checking load balancer"
    invalid_cert_test_load_balancer
    echo "...Checking gateway"
    invalid_cert_test_gateway "https://localhost:2000/events/data-collector"
}

invalid_cert_test_load_balancer() {
    local result
    result=$(hab_curl -sS --insecure --cert "$(invalid_cert_path)" --key "$(invalid_key_path)" "https://localhost/data-collector/v0")
    if ! echo "$result" | grep -q "SSL certificate error"; then
        cat <<EOF
        ...Failed
        Expected response to contain "SSL certificate error"

        Got:
        ${result}
EOF
    return 1
    else
        cat <<EOF
        ...Success
EOF
    fi
}

invalid_cert_test_gateway() {
    local result
    result=$(hab_curl -o /dev/null -sS --insecure --cert "$(invalid_cert_path)" --key "$(invalid_key_path)" "https://localhost:2000/events/data-collector" || echo "$?")
    if [ "$result" != "35" ]; then
        cat <<EOF
        ...Failed
        Expected curl exit code 35 (SSL connect error. The SSL handshaking failed.)

        Got:
        ${result}
EOF
    return 1
    else
        cat <<EOF
        ...Success
EOF
    fi
}


test_authorized_cert() {
    log_info "Running test_authorized_cert"
    cat <<EOF
    This test makes sure that a authorized cert that is signed
    by the A2 deployment CA and has been granted permissions is
    accepted
EOF

    authorized_cert_test "https://localhost/data-collector/v0"
    authorized_cert_test "https://localhost:2000/events/data-collector"
}

authorized_cert_test() {
    local endpoint="$1"
    echo "...Checking ${endpoint}"

    local result
    result=$(hab_curl -o /dev/null -w "%{http_code}" -sS --insecure --cert "$(authorized_cert_path)" --key "$(authorized_key_path)" "${endpoint}" )
    if [ "$result" != "200" ]; then
        cat <<EOF
        ...Failed
        Expected http code 200

        Got:
        ${result}
EOF
    return 1
    else
        cat <<EOF
        ...Success
EOF
    fi
}

test_authorized_cert_in_header_unauthenticated() {
    log_info "Running test_authorized_cert_in_header_unauthenticated"
    cat <<EOF
    This test makes sure that we do not use a certificate passed by the
    user in a header. Only the load balancer is allowed to set it.
EOF
    authorized_cert_in_header_unauthenticated_test "https://localhost/data-collector/v0"
    authorized_cert_in_header_unauthenticated_test "https://localhost:2000/events/data-collector"
}

authorized_cert_in_header_unauthenticated_test() {
    local endpoint="$1"
    echo "...Checking ${endpoint}"

    local result
    result=$(hab_curl -o /dev/null -w "%{http_code}" -sS --insecure \
        -H "X-Client-Cert: $(urlencode "$(cat "$(authorized_cert_path)")")"  \
        "${endpoint}" )
    if [ "$result" != "401" ]; then
        cat <<EOF
        ...Failed
        Expected http code 401

        Got:
        ${result}
EOF
    return 1
    else
        cat <<EOF
        ...Success
EOF
    fi
}

test_authorized_cert_in_header_authenticated() {
    log_info "Running test_authorized_cert_in_header_authenticated"
    cat <<EOF
    This test makes sure that we do not use a certificate passed by the
    user in a header. Only the load balancer is allowed to set it. In this
    scenario, we will authenticate with a valid cert, and pass a cert that
    is authorized to hit the endpoint. The authenticated cert is not authorized
EOF
    authorized_cert_in_header_authenticated_test "https://localhost/data-collector/v0"
    authorized_cert_in_header_authenticated_test "https://localhost:2000/events/data-collector"
}

authorized_cert_in_header_authenticated_test() {
    local endpoint="$1"
    echo "...Checking ${endpoint}"

    local result
    result=$(hab_curl -o /dev/null -w "%{http_code}" -sS --insecure \
        --cert "$(authenticated_cert_path)" --key "$(authenticated_key_path)" \
        -H "X-Client-Cert: $(urlencode "$(cat "$(authorized_cert_path)")")"  \
        "${endpoint}" )
    if [ "$result" != "403" ]; then
        cat <<EOF
        ...Failed
        Expected http code 403

        Got:
        ${result}
EOF
    return 1
    else
        cat <<EOF
        ...Success
EOF
    fi
}

authorized_service_name() {
    echo "automate-ui"
}

invalid_cert_path(){
    echo "${A2_ROOT_DIR}/dev/certs/$(authorized_service_name).crt"
}

invalid_key_path() {
    echo "${A2_ROOT_DIR}/dev/certs/$(authorized_service_name).key"
}

authorized_cert_path(){
    echo "/hab/svc/$(authorized_service_name)/config/service.crt"
}

authorized_key_path() {
    echo "/hab/svc/$(authorized_service_name)/config/service.key"
}

# We will not authorize teams-service to do anything, but it will
# still be authenticated
authenticated_service_name() {
    echo "teams-service"
}

authenticated_cert_path(){
    echo "/hab/svc/$(authenticated_service_name)/config/service.crt"
}

authenticated_key_path() {
    echo "/hab/svc/$(authenticated_service_name)/config/service.key"
}


urlencode() {
    # shamelessly stolen from
    # https://gist.github.com/cdown/1163649

    # urlencode <string>
    old_lc_collate=$LC_COLLATE
    LC_COLLATE=C

    local length="${#1}"
    for (( i = 0; i < length; i++ )); do
        local c="${1:i:1}"
        case $c in
            [a-zA-Z0-9.~_-]) printf "%s" "$c" ;;
            *) printf '%%%02X' "'$c" ;;
        esac
    done

    LC_COLLATE=$old_lc_collate
}
