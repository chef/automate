#!/bin/bash

#shellcheck disable=SC2034
test_name="iam_v1_force_upgrade_to_v2"
test_upgrades=true
test_upgrade_strategy="none"
# TODO decide what inspec profiles we want to test before AND after force-upgrade
# test_deploy_inspec_profiles=(a2-deploy-integration)
# test_upgrade_inspec_profiles=(a2-iam-v2-integration)
# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

# on this version, Automate had upgrade-to-v2 and the first three v2 data migrations
OLD_VERSION=20190501153509
OLD_MANIFEST_DIR="${A2_ROOT_DIR}/components/automate-deployment/testdata/old_manifests/"
DEEP_UPGRADE_PATH="${OLD_MANIFEST_DIR}/${OLD_VERSION}.json"

TEAM_ID="force-upgrade-inspec-test-team"

hab_curl() {
    hab pkg exec core/curl curl "$@"
}

do_deploy() {
    #shellcheck disable=SC2154
    cp "$DEEP_UPGRADE_PATH" "$test_manifest_path"

    # we make sure to use the CLI for the old version of Automate we want to deploy
    local cli_bin="/bin/chef-automate-${OLD_VERSION}"

    download_cli "${OLD_VERSION}" "${cli_bin}"

    #shellcheck disable=SC2154
    "${cli_bin}" deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa \
        --skip-preflight \
        --debug
}

do_test_deploy() {
  local token
  token=$(chef-automate admin-token)
  hab_curl -fsS -k -H "api-token: $token" "https://localhost/api/v0/auth/teams" \
    -d "$(jo name=$TEAM_ID description=testing only)"
    
  hab_curl -fsS -k -H "api-token: $token" "https://localhost/api/v0/auth/teams/$TEAM_ID"
    if grep -q "Error" <<< "$output"; then
    log_error "expected team, got output:"
    log_error "$output"
    return 1
  fi
}

do_test_upgrade() {
  local token
  token=$(chef-automate iam token create ADMIN_TEST --admin)

  local output="$(hab_curl -fsS -k -H "api-token: $token" "https://localhost/api/v0/auth/teams/$TEAM_ID")"
  if ! grep -q "Error" <<< "$output"; then
    log_error "expected error because this API has been decommissioned, got output:"
    log_error "$output"
    return 1
  fi

  output="$(hab_curl -fsS -k -H "api-token: $token" "https://localhost/apis/iam/v2/teams/$TEAM_ID")"
  if grep -q "Error" <<< "$output"; then
    log_error "expected team, got output:"
    log_error "$output"
    return 1
  fi
  
  output="$(hab_curl -fsS -k -H "api-token: $token" "https://localhost/apis/iam/v2/teams/$TEAM_ID")" \
        -XDELETE
  if grep -q "Error" <<< "$output"; then
    log_error "expected successful delete, got output:"
    log_error "$output"
    return 1
  fi
}

# TODO 
  # decide what inspec suites/diagnostics need to run in each test scenario (v1->v2, v2-no-legacy->v2, v2-legacy->v2)

  # do_test_deploy_default - this will take care of running inspec suites 
  # we specify in test_deploy_inspec_profiles variable

  # do_test_upgrade_default - this will take care of running inspec suites 
  # we specify in test_upgrade_inspec_profiles variable

  # test anything else we expect after the force-upgrade (might need new inspec tests?)
