#!/bin/bash

#shellcheck disable=SC2034
test_name="backup-s3"
test_backup_restore=true

__base_path() {
    if [ -z "$BUILDKITE_BUILD_ID" ]; then
        head /dev/urandom | tr -dc A-Za-z0-9 | head -c 13
    else
        echo "$BUILDKITE_BUILD_ID"
    fi
    return 0
}

bucket_name="a2-backup-restore-test"
base_path="$(__base_path)"
# deployment-service currently requires the regional endpoint
s3_endpoint="https://s3.us-west-2.amazonaws.com"

do_setup() {
    do_setup_default
}

do_create_config() {
    do_create_config_default

    #shellcheck disable=SC2154
    cat <<EOF >> "$test_config_path"
[global.v1.backups]
  location = "s3"
[global.v1.backups.s3.bucket]
  name = "${bucket_name}"
  base_path = "${base_path}"
  endpoint = "${s3_endpoint}"
EOF
}

do_prepare_restore() {
    do_prepare_restore_default

    # Remove the default backup directory
    rm -r /var/opt/chef-automate/backups

    # make sure we can list the backups, and it contains
    # our backup
    #shellcheck disable=SC2154
    chef-automate backup list \
        --debug \
        "s3://${bucket_name}/${base_path}" | awk -v s="${test_backup_id}" 'BEGIN{r=1}; $0~s{r=0} 1; END{exit(r)}'
}

do_restore() {
    #shellcheck disable=SC2154
    chef-automate backup restore \
        --debug \
        --override-origin "$HAB_ORIGIN" \
        "s3://${bucket_name}/${base_path}/${test_backup_id}"
}

do_test_restore() {
    do_test_restore_default || return 1
    delete_backup_and_assert_idempotent
    verify_packages
}
