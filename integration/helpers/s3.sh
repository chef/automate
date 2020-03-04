#!/bin/bash

#shellcheck disable=SC2034

__bucket_base_path() {
    head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13
    return 0
}

s3_bucket_name="a2-backup-restore-test"

s3_bucket_base_path="$(__bucket_base_path)"

# deployment-service currently requires the regional endpoint
s3_endpoint="https://s3.us-west-2.amazonaws.com"

do_create_config_s3_default() {
  #shellcheck disable=SC2154
  cat <<EOF >> "$test_config_path"
[global.v1.backups]
  location = "s3"
[global.v1.backups.s3.bucket]
  name = "${s3_bucket_name}"
  base_path = "${s3_bucket_base_path}"
  endpoint = "${s3_endpoint}"
[global.v1.backups.s3.credentials]
  access_key = "$AWS_ACCESS_KEY_ID"
  secret_key = "$AWS_SECRET_ACCESS_KEY"
  session_token = "$AWS_SESSION_TOKEN"
EOF
}

do_deploy_s3_default() {
    #shellcheck disable=SC2154
    chef-automate deploy config.toml \
        --product automate \
        --product builder \
        --hartifacts "$test_hartifacts_path" \
        --override-origin "$HAB_ORIGIN" \
        --manifest-dir "$test_manifest_path" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa

    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"
}

do_test_deploy_s3_default() {
    #shellcheck disable=SC2154
    #shellcheck source=integration/helpers/bldr_tests.sh
    source "${source_dir}/helpers/bldr_tests.sh"

    do_test_deploy_default
    bldr_smoke_test
}

do_backup_s3_default() {
  echo "Backing up to S3"
  echo "  bucket: ${s3_bucket_name}/${s3_bucket_base_path}"
  echo "  endpoint: ${s3_endpoint}"
  do_backup_default
}

do_prepare_restore_s3_default() {
  do_prepare_restore_default

  # Remove the default backup directory
  [[ -d /var/opt/chef-automate/backups ]] && rm -r /var/opt/chef-automate/backups

  # make sure we can list the backups, and it contains
  # our backup
  #shellcheck disable=SC2154
  chef-automate backup list \
      --debug \
      "s3://${s3_bucket_name}/${s3_bucket_base_path}" | awk -v s="${test_backup_id}" 'BEGIN{r=1}; $0~s{r=0} 1; END{exit(r)}'
}

do_restore_s3_default() {
  #shellcheck disable=SC2154
  chef-automate backup restore \
      --debug \
      --override-origin "$HAB_ORIGIN" \
      "s3://${s3_bucket_name}/${s3_bucket_base_path}/${test_backup_id}" \
      --s3-access-key "$AWS_ACCESS_KEY_ID" \
      --s3-secret-key "$AWS_SECRET_ACCESS_KEY" \
      --s3-session-token "$AWS_SESSION_TOKEN"
}

do_test_restore_s3_default() {
  do_test_restore_default || return 1
  delete_backup_and_assert_idempotent
  verify_packages

  hab pkg install core/hab
  artifacts_dir=$(mktemp -d --suffix="bldr_smoke")
  # TODO(jaym): remove when we upgrade hab to a version
  # that supports hab pkg download
  SSL_CERT_FILE="/hab/svc/automate-load-balancer/data/$CONTAINER_HOSTNAME.cert" \
      hab pkg exec core/hab \
      hab pkg download \
      -u "https://$CONTAINER_HOSTNAME/bldr/v1" \
      -c "unstable" \
      --download-directory "$artifacts_dir" \
      "${TEST_ORIGIN}/${TEST_PKG}"

  rm -r "$artifacts_dir"
}
