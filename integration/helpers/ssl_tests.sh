# shellcheck disable=SC2039
# shellcheck disable=SC2164
run_ssl_scan() {
    install_testssl_prereqs
    if [[ ! -d "/testssl.sh" ]]; then
       pushd "/"
         hab pkg exec core/git git clone --depth 1 https://github.com/drwetter/testssl.sh.git
       popd
    fi
    local testssl_last_commit
    pushd /testssl.sh
      testssl_last_commit=$(hab pkg exec core/git git log -1 --format=%cd)
      fixup_testssl
      hostfile=$(write_host_file)
      result_file=$(mktemp)
      run_testssl "$hostfile" "$result_file"
    popd
    rm "$hostfile"

    if command -v buildkite-agent; then
        buildkite-agent artifact upload "$result_file"
    fi

    filtered_violation_data=$(hab pkg exec core/jq-static jq -f integration/helpers/ssl_filters.jq "$result_file")
    violations=$(echo "$filtered_violation_data"| hab pkg exec core/jq-static jq length)
    if [[ "$violations" != "0" ]]; then
        log_error "== SSL CONFIGURATION TEST FAILED =="
        log_error "Found $violations SSL config violations not whitelisted by ssl_filters.jq file"
        log_error "Test failures may be caused by changes to https://github.com/drwetter/testssl.sh"
        log_error "testssl.sh was last changed on $testssl_last_commit"
        log_error "Violations (excluding whitelisted):"
        echo "$filtered_violation_data"
        return 1
    else
        log_line "SSL Quality Check Passed"
        return 0
    fi

    # Test for old TLS protocols
    #
    # This is an incomplete test since other TLS connection errors can
    # cause it to erroneously pass. But, it is a start.
    proto_check_failed=0
    while read -r host; do
        if check_tls_version ssl2 "$host"; then
            log_error "$host appears to offer SSLv2"
            proto_check_failed=1
        fi

        if check_tls_version ssl3 "$host"; then
            log_error "$host appears to offer SSLv3"
            proto_check_failed=1
        fi

        if check_tls_version tls1 "$host"; then
            log_error "$host appears to offer TLSv1.0"
            proto_check_failed=1
        fi

        if check_tls_version tls1_1 "$host"; then
            log_error "$host appears to offer TLSv1.1"
            proto_check_failed=1
        fi
    done < "$HOSTFILE"

    return "$proto_check_failed"
}

check_tls_version() {
    local version=$1
    local host=$2
    local cert_path="/hab/svc/deployment-service/data/deployment-service.crt"
    local key_path="/hab/svc/deployment-service/data/deployment-service.key"

    echo "Q" | hab pkg exec core/openssl openssl s_client -connect "$host" "-$version" -cert "$cert_path" -key "$key_path" 1>/dev/null 2>&1
}

run_testssl() {
    mypath=$(hab pkg path core/coreutils)/bin
    mypath="$mypath:$(hab pkg path core/procps-ng)/bin"
    mypath="$mypath:$(hab pkg path core/net-tools)/bin"
    mypath="$mypath:$PATH"
    PATH="$mypath" ./testssl.sh --parallel --quiet -n none --color 0 --severity LOW --warnings off --jsonfile "$2" --fast --add-ca /hab/svc/deployment-service/data/root.crt --file "$1"
}

install_testssl_prereqs() {
    hab pkg install core/bash
    hab pkg install core/git
    hab pkg install core/procps-ng
    hab pkg install core/coreutils
    hab pkg install core/jq-static
    hab pkg install core/net-tools
    hab pkg install core/curl
}

fixup_testssl() {
    #shellcheck disable=SC1117
    sed -ibak "1s%.*%#\!$(hab pkg path core/bash)/bin/bash%" ./testssl.sh
    chmod +x ./testssl.sh
}

write_host_file() {
    HOSTFILE=$(mktemp)
    IP=$(hab pkg exec core/curl curl -v http://localhost:9631/services/deployment-service/default | hab pkg exec core/jq-static jq -r .sys.ip)
    # temporarily remove port 10120 from list until it is a running service
    chef-automate dev ports list | awk "/https|grpc/ { print \"$IP:\" \$3 }" > "$HOSTFILE"
    echo "$HOSTFILE"
}
