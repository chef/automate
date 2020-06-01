#!/bin/bash

#shellcheck disable=SC2034
#shellcheck disable=SC2154

# shellcheck source=integration/helpers/gcs.sh
source "${source_dir}/helpers/gcs.sh"

test_name="backup-gcs"
test_backup_restore=true

do_create_config() {
  do_create_config_default
  do_create_config_gcs_default
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
