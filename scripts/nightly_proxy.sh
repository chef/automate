#!/usr/bin/env bash

set -e

dump_logs() {
    errcode=$?
    mkdir -p logs

    echo "Collecting logs"
    make proxy-gather-logs
    if command -v buildkite-agent
    then
        if [ "$errcode" -ne 0 ]; then
            echo "Deploy Failed - Uploading logs to buildkite"
        fi

        if ! buildkite-agent artifact upload "proxy/logs/*"
        then
            echo "Failed to upload logs... Dumping"
            cat proxy/logs/journal
        fi
    fi

    make proxy-down
    echo "Exiting with $errcode"
    exit $errcode
}

cd components/automate-deployment
trap dump_logs EXIT
sudo make proxy-host-setup
make proxy-build
curl -O https://packages.chef.io/files/dev/latest/chef-automate-cli/chef-automate_linux_amd64.zip
unzip -o chef-automate_linux_amd64.zip -d proxy
make proxy-up
sleep 10
make proxy-deploy
make proxy-diagnostics
