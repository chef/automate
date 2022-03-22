#!/bin/bash

#shellcheck disable=SC2034
test_name="upgrade_current_master_habdev"
test_upgrades=true
test_upgrade_inspec_profiles=()
test_upgrade_begin_manifest="current.json"

do_prepare_deploy() {
    do_prepare_deploy_default
    mkdir /etc/systemd/system/chef-automate.service.d
    cat > /etc/systemd/system/chef-automate.service.d/custom.conf <<EOF
[Service]
Environment=CHEF_DEV_ENVIRONMENT=true
EOF
}

do_prepare_upgrade() {
    do_prepare_upgrade_default
    set_test_manifest "build-habdev.json"
    set_version_file
}
