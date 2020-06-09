#!/bin/bash

#shellcheck disable=SC2034
#shellcheck disable=SC2154

# shellcheck source=integration/helpers/s3.sh
source "${source_dir}/helpers/s3.sh"

test_name="backup-external-es-s3"
test_backup_restore=true

do_setup() {
  do_setup_default

  local previous_umask
  previous_umask=$(umask)
  umask 022

  start_external_elasticsearch "s3" "${s3_endpoint}"

  umask "$previous_umask"
}

do_create_config() {
  do_create_config_default
  do_create_config_s3_default
  cat <<EOF >> "$test_config_path"
[global.v1.external.elasticsearch]
  enable = true
  nodes = ["http://127.0.0.1:59200"]
[global.v1.external.elasticsearch.backup]
  enable = true
  location = "s3"
EOF
}

do_deploy() {
  do_deploy_s3_default
}

do_test_deploy() {
  do_test_deploy_s3_default
}

do_backup() {
  do_backup_s3_default
}

do_prepare_restore() {
  do_prepare_restore_s3_default
}

do_restore() {
  do_restore_s3_default
}

do_test_restore() {
  do_test_restore_s3_default
}
