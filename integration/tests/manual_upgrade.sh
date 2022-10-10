#!/bin/bash

#shellcheck disable=SC2154
#shellcheck disable=SC2034
test_name="manual upgrade"
test_upgrades=true
test_upgrade_strategy="none"
test_loadbalancer_url="https://localhost:4443"
upgrade_scaffold_pid_file="/tmp/upgrade-scaffold-pid"

do_setup() {
    hab pkg install core/jq-static
    do_setup_default
}

jq() {
    hab pkg exec core/jq-static jq "$@"
}

do_prepare_deploy() {
    do_prepare_deploy_default

    mkdir /etc/systemd/system/chef-automate.service.d
    cat > /etc/systemd/system/chef-automate.service.d/custom.conf <<EOF
[Service]
Environment=CHEF_AUTOMATE_SKIP_MANIFEST_VERIFICATION=true
EOF
}

do_deploy() {
    set_test_manifest "current.json"
    #shellcheck disable=SC2154
    download_version "current" "$test_manifest_dir/current.json"
    set_test_versions "current.json-versions"
    upgrade_scaffold_bin="$(a2_root_dir)/components/automate-deployment/bin/linux/upgrade-test-scaffold"
    #shellcheck disable=SC2154
    $upgrade_scaffold_bin setup "$test_manifest_path"
    #shellcheck disable=SC2154
    $upgrade_scaffold_bin serve "$test_manifest_path" "$upgrade_scaffold_pid_file" "$test_versions_path" &
    sleep 5
    export CHEF_AUTOMATE_SKIP_MANIFEST_VERIFICATION=true
    export GODEBUG=x509ignoreCN=0
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

    set_version_file
    #shellcheck disable=SC2154
    jq --arg val "$release" '. + [$val]' "$versionsFile" > tmp.$$.json && mv tmp.$$.json "$versionsFile"

    curl -vv --insecure "https://packages.chef.io/set/$release" -X POST -d @"$target_manifest"
    log_info "Upgrading to $release"
    # Uncomment once the --version flag is on dev
    ERROR=$(chef-automate upgrade run  --version "$release" 2>&1 >/dev/null) || true
    echo "$ERROR"
    if echo "${ERROR}" | grep 'This is a Major upgrade'; then
        echo "major normal upgrade"
        echo "n
n" | chef-automate upgrade run --major  --version "$release"
        sleep 45
        #shellcheck disable=SC2154
        wait_for_upgrade "false"
        wait_for_healthy
        echo 'y
y' | chef-automate upgrade status --versions-file "$versionsFile"
    else
        echo "regular normal upgrade"
        sleep 45
        #shellcheck disable=SC2154
        wait_for_upgrade "false"
    fi
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
    chef-automate upgrade status --versions-file "$versionsFile"
    chef-automate upgrade status --versions-file "$versionsFile" | grep "Chef Automate upgraded to version: $release"
    do_test_upgrade_default
}
