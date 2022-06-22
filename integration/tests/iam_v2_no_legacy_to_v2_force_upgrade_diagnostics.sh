#!/bin/bash

# this test script:
# 1. deploys an older version of Automate and upgrades it to v2 using the beta CLI,
#    skipping v1 policy migration.
# 2. runs diagnostics without cleaning up the data. all v1 diagnostics are skipped 
#    because the system is on v2.
# 3. upgrades Automate to the latest build. This force-upgrades the system to IAM v2.
# 4. runs diagnostics, verify and cleanup steps only. 
#    all v1 diagnostics are skipped.

#shellcheck disable=SC2034
test_name="iam_force_upgrade_to_v2_with_no_legacy"
test_upgrades=true
test_upgrade_strategy="none"
test_diagnostics_pre_upgrade_filters="~skip-for-deep-upgrade"

# Note: we can't run diagnostics AND inspec, so we don't include any inspec tests
test_skip_diagnostics=false

# on this version, we released IAM v2 GA
OLD_VERSION=20200127203438
OLD_MANIFEST_DIR="${A2_ROOT_DIR}/components/automate-deployment/testdata/old_manifests/"
DEEP_UPGRADE_PATH="${OLD_MANIFEST_DIR}/${OLD_VERSION}.json"

do_deploy() {
    #shellcheck disable=SC2154
    cp "$DEEP_UPGRADE_PATH" "$test_manifest_path"

    # we use the CLI for the old version of Automate we want to deploy
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

    "${cli_bin}" iam upgrade-to-v2 --skip-policy-migration
}

do_prepare_upgrade() {
  # use latest current here
  prepare_upgrade_milestone "current" "20220329091442"
}
