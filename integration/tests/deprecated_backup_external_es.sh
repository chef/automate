#!/bin/bash

#shellcheck disable=SC2034
test_name="deprecated-backup-external-es"
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
[opensearch.v1.sys.node]
master=false
data=false

[opensearch.v1.sys.cluster]
name = "external-network"
[opensearch.v1.sys.network]
host = "0.0.0.0"
[opensearch.v1.sys.transport]
port = "10168-10169"
[opensearch.v1.sys.discovery]
ping_unicast_hosts = '''["127.0.0.1:59300"]'''
EOF
}
