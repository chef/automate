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

    do_apply_license
    echo "applying license after deployment"
    # chef-automate license status
}

do_prepare_upgrade() {
    set_version_file
    prepare_upgrade_milestone_append_version "current" "3.0.49"
}

run_upgrade() {
    local airgap_artifact_path="$1"

    # NOTE: This is a bit of a hack.
    #
    # The deployment-service determines the upgrade status by comparing the package
    # versions in it's manifest with the package versions that are currently
    # installed and running. The manifest is cached and only upgrade periodically.
    # To trigger an upgrade we need the manifest to include our new hartifacts.
    # Moving the hartifacts into the directory won't trigger a manifest rebuild
    # because that directory isn't watched for changes. Therefore, we'll trigger
    # a manifest rebuild with the run command.
    if [ -z "$airgap_artifact_path" ]; then
        # shellcheck disable=SC2154
        cat "$versionsFile"
        ERROR=$(chef-automate upgrade run --versions-file "$versionsFile" 2>&1 >/dev/null) || true
        echo "$ERROR"
        if echo "${ERROR}" | grep 'This is a Major upgrade'; then
            echo "major normal upgrade"
            echo "y
y
y
y
y
y" | chef-automate upgrade run --major --versions-file "$versionsFile"
            # NOTE: This is a hack
            # The hack above was no longer good enough because we have a thing that needs
            # to be updated that isn't a service
            sleep 45

            #shellcheck disable=SC2154
            wait_for_upgrade "$test_detect_broken_cli" "$test_detect_broken_packages"
            chef-automate post-major-upgrade migrate --data=PG -y
        else
            echo "regular normal upgrade"
            sleep 45

            #shellcheck disable=SC2154
            wait_for_upgrade "$test_detect_broken_cli" "$test_detect_broken_packages"
        fi
    fi
}