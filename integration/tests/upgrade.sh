test_name="upgrade"
test_upgrades=true
test_upgrade_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

