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

log_section_start() {
    echo "--- [$(date -u)] $*"
}

# shellcheck disable=2153
if [[ -n "$A1_LICENSE" ]]; then
    echo -e "$A1_LICENSE" | base64 --decode > components/automate-deployment/a1-migration/delivery.license
fi

cd components/automate-deployment
trap dump_logs EXIT

export HAB_LICENSE=accept-no-persist
log_section_start "Setting up A1 migration host"
sudo make a1-migration-host-setup
log_section_start "Building Docker containers"
make a1-migration-build
log_section_start "Installing automate-cli"
curl -O https://packages.chef.io/files/dev/latest/chef-automate-cli/chef-automate_linux_amd64.zip
unzip -o chef-automate_linux_amd64.zip -d a1-migration
log_section_start "Starting Automate 1"
make a1-migration-up
sleep 20 # TODO: Do we really need this sleep?
log_section_start "Loading Sample Data"
make a1-migration-load-sample-data
log_section_start "Migrating to Automate 2"
HARTIFACT_DIR="/a2/results" make a1-migration-migrate
log_section_start "Testing Automate 2"
# Password for a1-migration-data-full/0.0.1/20190530183952
A1_BUILDER_PASSWORD='KfK1LU/nGzk6J8BmnD+G3GwPn9+TqD5VGwQ=' AUTOMATE_API_DEFAULT_PASSWORD='WThqQgjnI6vAhi3TEVoM7GntWBI1HvJMREQ=' make a1-migration-test
