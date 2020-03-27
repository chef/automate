#!/bin/bash

#shellcheck disable=SC2034
test_name="backup"
test_backup_restore=true
test_diagnostics_opts="--opt compliance-report.days:120 --opt cfgmgmt-actions.days:120 --opt cfgmgmt-ccr.days:120"

do_deploy() {
    do_deploy_default
    log_info "applying dev license"
    chef-automate license apply "$A2_LICENSE"

    ## We define the purge policies to not delete any data we might generate
    chef-automate dev grpcurl compliance-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
      "enabled":true,
      "recurrence":"FREQ=DAILY;DTSTART=20190820T221315Z;INTERVAL=1",
      "policy_update": {
        "es": [
          {
            "disabled": false,
            "policy_name":"compliance-scans",
            "older_than_days":"150"
          },
          {
            "disabled": false,
            "policy_name":"compliance-reports",
            "older_than_days":"150"
          }
        ]
      }
    }'

    chef-automate dev grpcurl ingest-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
      "enabled":true,
      "recurrence":"FREQ=DAILY;DTSTART=20190820T221315Z;INTERVAL=1",
      "policy_update": {
        "es": [
          {
            "disabled": false,
            "policy_name":"converge-history",
            "older_than_days":"150"
          },
          {
            "disabled": false,
            "policy_name":"actions",
            "older_than_days":"150"
          }
        ]
      }
    }'

    chef-automate dev grpcurl event-feed-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
      "enabled":true,
      "recurrence":"FREQ=DAILY;DTSTART=20190820T221315Z;INTERVAL=1",
      "policy_update": {
        "es": [
          {
            "disabled": false,
            "policy_name":"feed",
            "older_than_days":"150"
          }
        ]
      }
    }'
}

do_restore() {
    test_metadata_sha256_mismatch_fails || return 1
    test_missing_checksums_file_fails || return 1
    do_restore_default
}

do_test_restore() {
    do_test_restore_default || return 1
    delete_backup_and_assert_idempotent
    test_delete_broken_backups
    test_can_regenerate_cert_after_restore

    # verifies IAM force-upgrade was applied
    # and the restored system demonstrates expected behavior around IAM permissions
    run_inspec_tests "${A2_ROOT_DIR}" "a2-iam-no-legacy-integration"
}
