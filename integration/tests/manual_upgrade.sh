#!/bin/bash

#shellcheck disable=SC2034
test_name="manual upgrade"
test_upgrades=true
test_upgrade_strategy="none"
test_loadbalancer_url="https://localhost:4443"
upgrade_scaffold_pid_file="/tmp/upgrade-scaffold-pid"
test_diagnostics_filters="~iam-v2"

do_setup() {
    hab pkg install core/jq-static
    do_setup_default
}

jq() {
    hab pkg exec core/jq-static jq "$@"
}


do_deploy() {
    set_test_manifest "current.json"
    upgrade_scaffold_bin="$(a2_root_dir)/components/automate-deployment/bin/linux/upgrade-test-scaffold"
    #shellcheck disable=SC2154
    $upgrade_scaffold_bin setup "$test_manifest_path"
    $upgrade_scaffold_bin serve "$test_manifest_path" "$upgrade_scaffold_pid_file" &
    sleep 5

    #shellcheck disable=SC2154
    echo -e "[load_balancer.v1.sys.service]\\nhttps_port = 4443" >> "$test_config_path"
    #shellcheck disable=SC2154
    /bin/chef-automate deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --upgrade-strategy "none" \
        --accept-terms-and-mlsa \
        --admin-password chefautomate \
        --skip-preflight \
        --debug

    chef-automate version
}

# By default, do_prepare_upgrade will replace the latest manifest.
# We don't want that. Instead, we want to test we can grab a specific release
do_prepare_upgrade() {
    #shellcheck disable=SC2154
    mv -f "$test_tmp_hartifacts_path"/* "$test_hartifacts_path/" || true
}

do_upgrade() {
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
    # chef-automate upgrade run --version "$release"
    chef-automate dev grpcurl deployment-service -- \
        chef.automate.domain.deployment.Deployment.Upgrade -d "{\"version\": \"$release\"}"
    wait_for_upgrade "false"
}

do_test_upgrade() {
    local release
    local target_manifest="$test_manifest_dir/build.json"
    release=$(jq -r .build <"$target_manifest")
    if [[ -z "$release" ]]
    then
        log_error "could not get release"
        return 1
    fi

    # Make sure the release is correct
    chef-automate upgrade status
    chef-automate upgrade status | grep "Automate is up-to-date ($release)"
    do_test_upgrade_default
}
