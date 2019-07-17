#!/bin/bash

#shellcheck disable=SC2034
test_name="backup-external-es"
test_backup_restore=true

do_setup() {
    do_setup_default

    local previous_umask
    previous_umask=$(umask)
    umask 022

    start_external_elasticsearch

    umask "$previous_umask"
}

do_create_config() {
    do_create_config_default

    #shellcheck disable=SC2154
    cat <<EOF >> "$test_config_path"
[global.v1.external.elasticsearch]
enable = true
nodes = ["http://127.0.0.1:59200"]

[global.v1.external.elasticsearch.backup]
enable = true
location = "fs"

[global.v1.external.elasticsearch.backup.fs]
path = "/var/opt/chef-automate/backups"
EOF
}
