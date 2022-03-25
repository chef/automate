#!/bin/bash

# this test script:
# 1. deploys an older version of Automate on IAM v1
# 2. runs diagnostics without cleaning up the data. all v2 diagnostics are skipped 
#    because the system is on v1.
# 3. upgrades Automate to the latest build. This force-upgrades the system to IAM v2.
# 4. runs diagnostics, verify and cleanup steps only. 
#    since projects and roles were not generated, it skips those two. 
#    the v2 policies diagnostic verifies the v1 policy that was migrated to v2. 
#    the v1 policy diagnostic is skipped because the system is on v2.

#shellcheck disable=SC2034
test_name="iam_v1_force_upgrade_to_v2_diagnostics"
test_upgrades=true
test_upgrade_strategy="none"
test_diagnostics_pre_upgrade_filters="~skip-for-deep-upgrade"

# Note: we can't run diagnostics AND inspec, so we don't include any inspec tests
test_skip_diagnostics=false

# on this version, Automate had upgrade-to-v2 and the first three v2 data migrations
OLD_VERSION=20190501153509
OLD_MANIFEST_DIR="${A2_ROOT_DIR}/components/automate-deployment/testdata/old_manifests/"
DEEP_UPGRADE_PATH="${OLD_MANIFEST_DIR}/${OLD_VERSION}.json"

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

do_prepare_upgrade() {
  # use latest current here
  download_manifest_version "current" "20220121191356" "$test_manifest_dir/20220121191356.json"
  set_test_manifest "20220121191356.json"
}
