test_name="deep_upgrade"
test_upgrades=true
test_upgrade_strategy="none"
test_loadbalancer_url="https://localhost:4443"
upgrade_scaffold_pid_file="/tmp/upgrade-scaffold-pid"
test_diagnostics_filters="~skip-for-deep-upgrade"

# The version deployed here has a bug where it's possible for it to
# serve a CLI that is incomplete. We can't go back in time and fix it,
# but we'll detect this and ignore it if it happens
test_detect_broken_cli=true

# The version deployed here has a bug where it's possible for package
# installations to be interrupted by a deployment-service restart,
# leaving the packages in a corrupt state. We can't go back in time
# and fix it, but we try to identify common cases and fix them in the
# tests.
test_detect_broken_packages=true

CURRENT_OLDEST_VERSION=20180519154949
OLD_MANIFEST_DIR="${A2_ROOT_DIR}/components/automate-deployment/testdata/old_manifests/"
DEEP_UPGRADE_PATH="${OLD_MANIFEST_DIR}/${CURRENT_OLDEST_VERSION}.json"

do_deploy() {
    cat $DEEP_UPGRADE_PATH > $test_manifest_path
    install_hab "0.54.0"
    upgrade_scaffold_bin="$(a2_root_dir)/components/automate-deployment/bin/linux/upgrade-test-scaffold"
    $upgrade_scaffold_bin setup $test_manifest_path
    $upgrade_scaffold_bin serve $test_manifest_path $upgrade_scaffold_pid_file &
    sleep 5

    echo -e "[load_balancer.v1.sys.service]\nhttps_port = 4443" >> $test_config_path
    /bin/chef-automate deploy "$test_config_path" \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --upgrade-strategy "$test_upgrade_strategy" \
        --accept-terms-and-mlsa \
        --admin-password chefautomate \
        --skip-preflight \
        --debug
}

do_cleanup() {
    kill "$(cat $upgrade_scaffold_pid_file)"
}
