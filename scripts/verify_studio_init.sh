#!/usr/bin/env sh
#
# NOTE: This script is likely sourced in /bin/sh and not bash. Please
# keep code in this script sh-compatible.
#
# verify_studio_init: Setup a `hab studio run` environment to run
# tests in CI.
#
# This script is intended for sourcing directly rather than executing.
#
SUP_LOG_FILE=/hab/sup/default/sup.log

#shellcheck disable=SC1091
. .studiorc

scripts/copy_hartifacts.sh results

copy_logs() {
    log_info "Copying supervisor log into results directory"
    if [ -f "$SUP_LOG_FILE" ]; then
        mkdir -p /src/results/logs/
        cp "$SUP_LOG_FILE" /src/results/logs/sup.log
    else
        log_warning "NO SUP LOG FOUND"
    fi
    # call our standard cleanup function
    cleanup
}

trap copy_logs EXIT
