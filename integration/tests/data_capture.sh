#!/bin/bash
#shellcheck disable=SC2034
test_name="gather-logs data capture"
test_upgrades=false
test_upgrade_strategy="none"
test_deploy_inspec_profiles=()
test_skip_diagnostics=true

export archive_file breadcrumb_file client_body_path error_count nginx_conf
archive_file="/tmp/capture.tar.gz"
client_body_path="/hab/svc/automate-load-balancer/var/client-body"
breadcrumb_file="$client_body_path/CaptureW0rks"
error_count=0
nginx_conf="/hab/svc/automate-load-balancer/config/nginx.conf"

do_setup() {
    hab pkg install core/tar
    do_setup_default
}

abnormal_end() {
    return 1
}

hab_tar() {
    hab pkg exec core/tar tar "$@"
}

check_conf_has_item() {
    match="client_body_in_file_only $1;"
    if grep -q "$match" $nginx_conf
    then
        log_info "TEST OK - found nginx.conf is configured as expected"
    else
        log_error "TEST ERROR - did not find match in nginx.conf. Expected: '$match'"
        (( ++error_count ))
    fi
}

do_test_deploy() {
    before="/tmp/data-capture-nginx.conf.before"
    after="/tmp/data-capture-nginx.conf.after"

    touch "$breadcrumb_file"

    cp -f "$nginx_conf" "$before"

    check_conf_has_item "off"
    chef-automate gather-logs -c -t 10 -y -o $archive_file &

    # pause a moment for nginx.conf to be updated
    sleep 5
    check_conf_has_item "on"

    counter=0

    while [[ -n $(jobs -r) ]]; do
        if [[ "$counter" -gt 20 ]]; then
            log_error "TEST ERROR - max loop iterations exceeded ($counter); Exiting loop!"
            (( ++error_count ))
            break
        fi
        log_info "TEST STATUS - waiting for capture to complete.."
        sleep 1
        (( ++counter ))
    done

    check_conf_has_item "off"

    cp -f "$nginx_conf" "$after"

    if diff "$before" "$after"; then
        log_info "TEST OK - nginx.conf is identical to before the test"
    else
        log_error "TEST ERROR - nginx.conf was not restored correctly!"
        (( ++error_count ))
    fi

    if [ ! -e "$archive_file" ]; then
        log_error "TEST ERROR - $archive_file not found!"
        (( ++error_count ))
    elif ! hab_tar tvf "$archive_file" "*$breadcrumb_file"; then
        log_error "TEST ERROR - the archive created did not contain $breadcrumb_file"
        (( ++error_count ))
    fi

    if [ -n "$(ls -A $client_body_path)" ]; then
        log_error "TEST ERROR - $client_body_path still contains files!"
        (( ++error_count ))
    fi

    rm -f "$archive_file" "$breadcrumb_file"

    if [ $error_count -gt 0 ]; then
        log_info "Total Errors: $error_count"
        abnormal_end
    fi

    do_test_deploy_default
}
