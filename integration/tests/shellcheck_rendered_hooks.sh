#!/bin/bash

#shellcheck disable=SC2034
test_name="shellcheck hooks"
test_deploy_inspec_profiles=()
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

    hab pkg install -b core/shellcheck

    umask "$previous_umask"
}

do_test_deploy() {
    shellcheck -s bash -x /hab/svc/*/hooks/*
}
