#!/bin/bash

# this test script:
# 1. deploys an older version of Automate on IAM v1.
# 2. runs inspec tests to verify IAM v1 behavior.
# 3. upgrades Automate to the latest build. This force-upgrades the system to IAM v2.
# 4. runs inspec tests to verify that the system was not disrupted by the force-upgrade
#    and legacy policies continue to be enforced.

#shellcheck disable=SC2034
test_name="iam_v1_force_upgrade_to_v2"
test_upgrades=true
test_upgrade_strategy="none"

# a2-iam-v1-integration verifies default policy permissions on an IAM v1 system
test_deploy_inspec_profiles=(a2-iam-v1-integration)

# a2-deploy-integration verifies that the system is up and all APIs work correctly
# (which now includes only IAM v2 APIs)
# a2-iam-legacy-integration verifies that v1 default policies were migrated 
# and their permissions are enforced just like on v1
test_upgrade_inspec_profiles=(a2-deploy-integration a2-iam-legacy-integration)

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

do_upgrade() {

    log_info "Downloading channel manifests"
    download_manifest_version "dev" "$test_manifest_dir/dev.json" "20220131135806"
    download_manifest_version "acceptance" "$test_manifest_dir/acceptance.json" "20220121191356"
    download_manifest_version "current" "$test_manifest_dir/current.json" "20220121191356"
    log_info "Creating build.json"
    create_manifest "$test_manifest_milestone_dir/build.json"

    local release target_manifest
    #shellcheck disable=SC2154
    target_manifest="$test_manifest_dir/build.json"
    release=$(jq -r .build <"$target_manifest")
    if [[ -z "$release" ]]
    then
        log_error "could not get release"
        return 1
    fi

    curl -vv --insecure "https://packages.chef.io/set/$release" -X POST -d @"$target_manifest"
    log_info "Upgrading to $release"
    # Uncomment once the --version flag is on dev
    # chef-automate upgrade run --version "$release"
    chef-automate dev grpcurl deployment-service -- \
        chef.automate.domain.deployment.Deployment.Upgrade -d "{\"version\": \"$release\"}"
    wait_for_upgrade "false"
}
