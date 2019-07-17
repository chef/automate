#!/bin/bash

test_name="backup_repo_permissions"
test_skip_diagnostics=true

do_test_deploy() {
  # Make sure the default settings work as expected
  # do_test_deploy_default

  local base_backup_dir="/tmp/$test_build_slug/backups"
  local hab_backup_dir="$base_backup_dir/hab"
  local root_backup_dir="$base_backup_dir/root"
  local hab_backup_cfg="/tmp/$test_build_slug/hab_backup.toml"
  local root_backup_cfg="/tmp/$test_build_slug/root_backup.toml"
  local out
  local res
  local previous_umask
  previous_umask=$(umask)

  [[ -d "$base_backup_dir" ]] && rm -rf "$base_backup_dir"

  # Create different fake backup directories for testing
  umask 022
  mkdir -p "$hab_backup_dir"
  mkdir -p "$root_backup_dir"
  chown hab:hab "$hab_backup_dir"
  chown root:root "$root_backup_dir"
  umask "$previous_umask"

  # Create valid and invalid configurations
  cat << EOF > "$hab_backup_cfg"
[global.v1.backups.filesystem]
  path = "${hab_backup_dir}"
EOF

  cat << EOF > "$root_backup_cfg"
[global.v1.backups.filesystem]
  path = "${root_backup_dir}"
EOF

  # Updating the backup dir with bad perms fails
  if out=$(chef-automate config patch "$root_backup_cfg" 2>&1); then
    return 1
  else
    grep "read/write/exec" <<< "$out" || return 1
  fi

  # Locally listing a backup dir with bad pers fails
  if out=$(chef-automate backup list "$root_backup_dir" 2>&1); then
    return 1
  else
    grep "read/write/exec" <<< "$out"
  fi

  # Commands with hab dir succeed
  chef-automate config patch --debug "$hab_backup_cfg"
  chef-automate backup list --debug "$hab_backup_dir"
  chef-automate backup create --debug
  test_backup_id=$(chef-automate backup list | tail -1 | awk '{print $1}')

  do_prepare_restore
  chef-automate backup restore --debug --yes --override-origin "$HAB_ORIGIN" "$test_backup_id"

  # restore with broken hab dir should fail
  chown root:root "$hab_backup_dir"
  do_prepare_restore
  if out=$(chef-automate backup restore --debug --yes --override-origin  "$HAB_ORIGIN" "$test_backup_id" 2>&1); then
    return 1
  else
    grep -v "read/write" <<< "$out"
  fi

  # fix-repo-permissions should fix broken hab-dir
  chef-automate backup fix-repo-permissions --debug "$hab_backup_dir"
  chef-automate backup restore --debug --yes --override-origin "$HAB_ORIGIN" "$test_backup_id"
}
