#!/usr/bin/env bash

set -e

dump_logs() {
    errcode=$?
    mkdir -p logs

    echo "Collecting logs"
    make a1-migration-gather-logs
    if command -v buildkite-agent
    then
        if [ "$errcode" -ne 0 ]; then
            echo "Deploy Failed - Uploading logs to buildkite"
        fi

        if ! buildkite-agent artifact upload "a1-migration/logs/*"
        then
            echo "Failed to upload logs... Dumping"
            cat a1-migration/logs/journal
        fi
    fi

    make a1-migration-down
    echo "Exiting with $errcode"
    exit $errcode
}

if [[ -n "$A1_LICENSE" ]]; then
    echo -e "$A1_LICENSE" | base64 --decode > components/automate-deployment/a1-migration/delivery.license
fi

cd components/automate-deployment
trap dump_logs EXIT
sudo make a1-migration-host-setup
make a1-migration-build
curl -O https://packages.chef.io/files/dev/latest/chef-automate-cli/chef-automate_linux_amd64.zip
unzip -o chef-automate_linux_amd64.zip -d a1-migration
make a1-migration-up
sleep 20 # TODO: Do we really need this sleep?
make a1-migration-load-sample-data
HARTIFACT_DIR="/a2/results" make a1-migration-migrate
# Password for a1-migration-data-full/0.0.1/20190530183952
A1_BUILDER_PASSWORD='KfK1LU/nGzk6J8BmnD+G3GwPn9+TqD5VGwQ=' AUTOMATE_API_DEFAULT_PASSWORD='WThqQgjnI6vAhi3TEVoM7GntWBI1HvJMREQ=' make a1-migration-test
