#!/bin/bash

#shellcheck disable=SC2034
test_name="ha_chef_server"
test_external_services=(ha_backend)

source integration/services/common.sh

_frontend1_container_name="$(service_container_name "cs1")"
_frontend2_container_name="$(service_container_name "cs2")"

do_setup() {
    do_setup_default

    echo "Installing docker"
    hab pkg install --binlink core/docker
    echo "Installed docker"
}

do_create_config() {
    do_create_config_default
    #shellcheck disable=SC2154
    cat /services/ha_backend.toml >> "$test_config_path"

    cat <<EOF >> "$test_config_path"
[erchef.v1.sys.data_collector]
enabled = false
EOF
}

do_deploy() {
    cli_bin=$(command -v "chef-automate")

    docker_run "${_frontend1_container_name}"
    docker_run "${_frontend2_container_name}"
    #shellcheck disable=SC2154
    docker exec -t "$_frontend1_container_name" \
        "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$test_hartifacts_path"
    docker exec -t "$_frontend2_container_name" \
        "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$test_hartifacts_path"


    #shellcheck disable=SC2154
    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" deploy config.toml \
            --product chef-server \
            --hartifacts "$test_hartifacts_path" \
            --override-origin "$HAB_ORIGIN" \
            --manifest-dir "$test_manifest_path" \
            --admin-password chefautomate \
            --accept-terms-and-mlsa

    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" bootstrap bundle create -o bootstrap.abb

    docker exec -t "$_frontend2_container_name" \
        "$cli_bin" deploy config.toml \
            --product chef-server \
            --hartifacts "$test_hartifacts_path" \
            --override-origin "$HAB_ORIGIN" \
            --manifest-dir "$test_manifest_path" \
            --admin-password chefautomate \
            --bootstrap-bundle bootstrap.abb \
            --accept-terms-and-mlsa

}

do_test_deploy() {
    docker exec --env "PATH=/hab/bin:/bin" -t "$_frontend2_container_name" chef-server-ctl test
    docker exec --env "PATH=/hab/bin:/bin" -t "$_frontend1_container_name" chef-server-ctl test
}

do_cleanup() {
    docker stop "$_frontend1_container_name"
    docker stop "$_frontend2_container_name"
}
