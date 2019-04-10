#!/usr/bin/env bash

# Test upgrading a non-all-in-one A1 install to A2.

set -e

dump_logs() {
    errcode=$?
    mkdir -p logs

    echo "Collecting logs"
    make basic-a1-gather-logs
    if command -v buildkite-agent
    then
        if [ "$errcode" -ne 0 ]; then
            echo "Deploy Failed - Uploading logs to buildkite"
        fi

        if ! buildkite-agent artifact upload "basic-a1/logs/*"
        then
            echo "Failed to upload logs... Dumping"
            cat basic-a1/logs/journal
        fi
    fi

    make basic-a1-down
    echo "Exiting with $errcode"
    exit $errcode
}

if [[ -n "$A1_LICENSE" ]]; then
    echo -e "$A1_LICENSE" | base64 --decode > components/automate-deployment/a1-migration/delivery.license
fi

cd components/automate-deployment
trap dump_logs EXIT
curl -O https://packages.chef.io/files/dev/latest/chef-automate-cli/chef-automate_linux_amd64.zip
unzip -o chef-automate_linux_amd64.zip -d basic-a1
make basic-a1-build
make basic-a1-start-chef-server
make basic-a1-up
make basic-a1-reconfigure-chef-server
make basic-a1-migrate
make basic-a1-diagnostics
make basic-a1-test
