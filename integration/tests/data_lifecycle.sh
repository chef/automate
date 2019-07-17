#!/bin/bash

#shellcheck disable=SC2034
test_name="data_lifecycle"

do_create_config() {
    do_create_config_default

    #shellcheck disable=SC2154
    cat <<EOF >> "$test_config_path"
[ingest.v1.sys.service]
purge_actions_after_days = 2
purge_converge_history_after_days = 2

[compliance.v1.sys.retention]
compliance_report_days = 2
EOF

}

do_test_deploy() {
    hab pkg exec "$HAB_ORIGIN"/data-lifecycle-service dls-e2eintegration
}
