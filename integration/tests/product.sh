test_name="product"
test_deploy_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

do_test_deploy() {
    do_test_deploy_default

    # make sure we can set the log level while things are running
    chef-automate debug set-log-level automate-gateway debug
    chef-automate debug set-log-level deployment-service debug

    source "${source_dir}/helpers/cert_auth_tests.sh"
    cert_auth_tests

    world_writable_files_test

    # We cannot use 'chef-automate stop' yet because
    # there is no way for that command to return successfully
    # I'd rather not jam a fix in
    log_info "Restarting chef-automate via chef-automate start/stop"
    chef-automate -d stop
    chef-automate -d start
    wait_for_healthy
    log_info "Done restarting chef-automate"

    verify_packages
}

world_writable_files_test() {
    # Check for world writable files
    log_info "checking for world writable files"
    matching="$(find /hab/ -xdev -perm -0002 -type f -print)"
    if [ ! -z "$matching" ]; then
        log_error "the following files are world writable:"
        echo "$matching"
        return 1
    fi
}

do_cleanup() {
    ps aux
    chef-automate uninstall -d --yes
}
