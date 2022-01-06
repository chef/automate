#!/usr/bin/env sh
#
# NOTE: This script is likely sourced in /bin/sh and not bash. Please
# keep code in this script sh-compatible.
#
# verify_setup: Initialize a buildkite host for running a `hab studio
# run` style integration test.
#
scripts/download_verify_harts.sh

upload_logs() {
    echo "Attempting to upload logs to buildkite"
    if ! buildkite-agent artifact upload "results/logs/*"; then
        echo "Failed to upload logs. Print to stdout instead"
        cat results/logs/*
    fi
}

trap upload_logs EXIT
