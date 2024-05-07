#!/bin/bash

#shellcheck disable=SC2034

__bucket_base_path() {
    head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13
    return 0
}

gcs_bucket_name="a2-backup-restore-test"
gcs_bucket_base_path="$(__bucket_base_path)"
gcs_creds_path="/tmp/gcpcred"

do_setup_gcs_default() {
  cat <<< "$GOOGLE_APPLICATION_JSON" > "${gcs_creds_path}"
}

do_create_config_gcs_default() {
  #shellcheck disable=SC2154
  cat <<EOF >> "$test_config_path"
[global.v1.backups]
  location = "gcs"
[global.v1.backups.gcs.bucket]
  name = "${gcs_bucket_name}"
  base_path = "${gcs_bucket_base_path}"
[global.v1.backups.gcs.credentials]
  json = '''
  $GOOGLE_APPLICATION_JSON
  '''
EOF
}

do_deploy_gcs_default() {
  #shellcheck disable=SC2154
  chef-automate deploy config.toml \
      --product automate \
      --hartifacts "$test_hartifacts_path" \
      --override-origin "$HAB_ORIGIN" \
      --manifest-dir "$test_manifest_path" \
      --admin-password chefautomate \
      --accept-terms-and-mlsa

  do_apply_license 

}

do_apply_license(){
    chef-automate license apply eyJhbGciOiJFUzUxMiIsInR5cCI6IkpXVCJ9.eyJpZCI6IjBiZjA4ZDM4LTdiYzQtNDRjMy1hMzYyLTA3ZDlmNzEwZmVkYyIsInZlcnNpb24iOiIxIiwidHlwZSI6ImludGVybmFsIiwiZ2VuZXJhdG9yIjoiY2hlZi9saWNlbnNlLTIuMC4wIiwia2V5X3NoYTI1NiI6ImUwZGYyOGM4YmM2ODE1MGVkYmZlZjk4Y2Q2YjdkYzQzOWMxZjgwYzdlN2VmNzQ3ODkzYTY4OTNhMmY3YjYwZjciLCJnZW5lcmF0aW9uX2RhdGUiOnsic2Vjb25kcyI6MTcxNTA1OTQ4Nn0sImN1c3RvbWVyIjoiY2hlZiBBdXRvbWF0ZSBpbnRlcm5hbCBsaWNlbnNlIiwiY3VzdG9tZXJfaWQiOiI0QjM5Q0Q3QS04QzEwLTRFNjAtQTUwQi1BNDNCODAyM0RCMTMiLCJjdXN0b21lcl9pZF92ZXJzaW9uIjoiMSIsImVudGl0bGVtZW50cyI6W3sibmFtZSI6IkNoZWYgQXV0b21hdGUiLCJtZWFzdXJlIjoibm9kZXMiLCJsaW1pdCI6LTEsInN0YXJ0Ijp7InNlY29uZHMiOjE3MTUwNDAwMDB9LCJlbmQiOnsic2Vjb25kcyI6MTc0NjU3NTk5OX19XX0.AEMHZjRSytD6rzejer-FzFQ84O0GBjdoQCinKFxAKY1nvUbhJIXCF5abBjOHYzZSvKe30XM5U232e9AnHUMGWJWKAcbEK-eMQiHy2iwB6mKpEq6hXuO27P0xZBUHkq0Rz3WpWry6NLVQqqJXAbE79hsOE-g4Ms4C-y1rEw0fkP19RJLQ
}


do_test_deploy_gcs_default() {
    do_test_deploy_default
}

do_backup_gcs_default() {
  echo "Backing up to GCS"
  echo "  bucket: ${gcs_bucket_name}/${gcs_bucket_base_path}"
  do_backup_default
}

do_prepare_restore_gcs_default() {
  do_prepare_restore_default

  # Remove the default backup directory
  [[ -d /var/opt/chef-automate/backups ]] && rm -r /var/opt/chef-automate/backups

  # make sure we can list the backups, and it contains
  # our backup
  #shellcheck disable=SC2154
  chef-automate backup list \
      --debug \
      --gcs-credentials-path "/tmp/gcpcred" \
      "gs://${gcs_bucket_name}/${gcs_bucket_base_path}" | awk -v s="${test_backup_id}" 'BEGIN{r=1}; $0~s{r=0} 1; END{exit(r)}'
}

do_restore_gcs_default() {
  #shellcheck disable=SC2154
  chef-automate backup restore \
      --debug \
      --override-origin "$HAB_ORIGIN" \
      --gcs-credentials-path "/tmp/gcpcred" \
      "gs://${gcs_bucket_name}/${gcs_bucket_base_path}/${test_backup_id}"
}

do_test_restore_gcs_default() {
  do_test_restore_default || return 1
  delete_backup_and_assert_idempotent
  verify_packages
}
