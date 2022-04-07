#!/bin/bash

#shellcheck disable=SC2034
test_name="v2_to_v3_upgrade"
test_upgrades=true
test_upgrade_strategy="none"

# a2-iam-no-legacy-integration verifies permissions on an IAM v2 system
# without v1 legacy policies
test_deploy_inspec_profiles=(a2-deploy-smoke)
test_skip_diagnostics=true

# here we use the latest milestone version
OLD_VERSION=20220329091442
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
