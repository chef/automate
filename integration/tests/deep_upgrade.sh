#!/bin/bash

#shellcheck disable=SC2034
test_name="deep_upgrade"
test_upgrades=true
test_upgrade_strategy="none"
test_loadbalancer_url="https://localhost:4443"
upgrade_scaffold_pid_file="/tmp/upgrade-scaffold-pid"
test_diagnostics_pre_upgrade_filters="~skip-for-deep-upgrade"
test_diagnostics_filters="~purge"

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

do_create_config() {
    log_info "Deferring configuration creation until do_deploy"
}

do_prepare_deploy() {
    do_prepare_deploy_default

    mkdir /etc/systemd/system/chef-automate.service.d
    cat > /etc/systemd/system/chef-automate.service.d/custom.conf <<EOF
[Service]
Environment=CHEF_AUTOMATE_SKIP_MANIFEST_VERIFICATION=true
EOF
}

do_upgrade() {
  test_detect_broken_packages=false
  do_upgrade_default
}

do_deploy() {
    #shellcheck disable=SC2154
    cp "$DEEP_UPGRADE_PATH" "$test_manifest_path"
    install_hab "0.54.0"
    upgrade_scaffold_bin="$(a2_root_dir)/components/automate-deployment/bin/linux/upgrade-test-scaffold"
    $upgrade_scaffold_bin setup "$test_manifest_path"
    $upgrade_scaffold_bin serve "$test_manifest_path" "$upgrade_scaffold_pid_file" &
    sleep 5

    log_info "Generating Automate configuration"
    #shellcheck disable=SC2154
    /bin/chef-automate init-config \
        --channel "$test_channel" \
        --file "$test_config_path" \
        --upgrade-strategy "$test_upgrade_strategy"
    cat <<EOF >>"$test_config_path"
[deployment.v1.sys.log]
  level = "debug"
[dex.v1.sys.expiry]
  id_tokens = "5m"
[postgresql.v1.sys.pg]
  shared_buffers = "1GB"
[load_balancer.v1.sys.service]
  https_port = 4443
EOF

    #shellcheck disable=SC2154
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
