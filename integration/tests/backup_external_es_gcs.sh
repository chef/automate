#!/bin/bash

#shellcheck disable=SC2034
#shellcheck disable=SC2154

# shellcheck source=integration/helpers/gcs.sh
source "${source_dir}/helpers/gcs.sh"

test_name="backup-external-es-gcs"
test_backup_restore=true

do_setup() {
  do_setup_default
  do_setup_gcs_default

  local previous_umask
  previous_umask=$(umask)
  umask 022

  start_external_elasticsearch "gcs" "${gcs_creds_path}"

  umask "$previous_umask"
}

do_create_config() {
  do_create_config_default
  do_create_config_gcs_default
  cat <<EOF >> "$test_config_path"
[global.v1.external.elasticsearch]
  enable = true
  nodes = ["http://127.0.0.1:59200"]
[global.v1.external.elasticsearch.backup]
  enable = true
  location = "gcs"
EOF
}

do_deploy() {
  do_deploy_gcs_default
}

do_test_deploy() {
  do_test_deploy_gcs_default
}

do_backup() {
  do_backup_gcs_default
}

do_prepare_restore() {
  do_prepare_restore_gcs_default
}

do_restore() {
  do_restore_gcs_default
}

do_test_restore() {
  do_test_restore_gcs_default
}
