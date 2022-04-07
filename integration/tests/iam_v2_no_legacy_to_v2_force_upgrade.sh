#!/bin/bash

# this test script:
# 1. deploys an older version of Automate and upgrades it to v2 using the beta CLI,
#    skipping v1 policy migration.
# 2. runs inspec tests to verify IAM v2 behavior without legacy policies.
# 3. upgrades Automate to the latest build. This force-upgrades the system to IAM v2.
# 4. runs inspec tests to verify that the system was not disrupted by the force-upgrade
#    and no legacy policies were migrated.

#shellcheck disable=SC2034
test_name="iam_force_upgrade_to_v2_with_no_legacy"
test_upgrades=true
test_upgrade_strategy="none"

# a2-iam-no-legacy-integration verifies permissions on an IAM v2 system 
# without v1 legacy policies
test_deploy_inspec_profiles=(a2-deploy-smoke)

# a2-deploy-integration verifies that the system is up and all APIs work correctly
# (which now includes only IAM v2 APIs)
# a2-iam-no-legacy-integration verifies permission enforcement on a fresh IAM v2
# system with no v1 legacy policies enforced
test_upgrade_inspec_profiles=(a2-deploy-integration a2-iam-no-legacy-integration)

# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

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
