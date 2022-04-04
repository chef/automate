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
    set_version_file
    do_prepare_upgrade_default
    set_test_manifest "build-habdev.json"
    append_version_file
}

run_upgrade() {
    #shellcheck disable=SC2154
    chef-automate upgrade run --versions-file "$versionsFile"

    # NOTE: This is a hack
    # The hack above was no longer good enough because we have a thing that needs
    # to be updated that isn't a service
    sleep 45

    #shellcheck disable=SC2154
    wait_for_upgrade "$test_detect_broken_cli" "$test_detect_broken_packages"
}
