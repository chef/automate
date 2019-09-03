#!/bin/bash

#shellcheck disable=SC2034
test_name="upgrade_current_master"
test_upgrades=true
test_upgrade_inspec_profiles=()
test_diagnostics_filters="~iam-v2 ~purge" # remove purge until 1219 makes it to current
test_upgrade_begin_manifest="current.json"
