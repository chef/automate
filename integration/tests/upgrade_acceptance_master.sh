#!/bin/bash

#shellcheck disable=SC2034
test_name="upgrade_acceptance_master"
test_upgrades=true
test_upgrade_inspec_profiles=()
test_upgrade_begin_manifest="acceptance.json"

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
