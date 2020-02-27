#!/bin/bash

#shellcheck disable=SC2034
test_name="iam_v1_force_upgrade_to_v2"
test_upgrades=true
test_upgrade_strategy="none"
test_deploy_inspec_profiles=(a2-iam-v1-integration)
test_upgrade_inspec_profiles=(a2-iam-legacy-integration a2-api-integration)
# Note: we can't run diagnostics AND inspec, so skip diagnostics
test_skip_diagnostics=true

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
