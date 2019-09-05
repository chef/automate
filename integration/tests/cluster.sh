#!/bin/bash

#shellcheck disable=SC2034
test_name="cluster"
test_external_services=(ha_backend)

source integration/services/common.sh

_frontend1_container_name="$(service_container_name "cluster1")"
_frontend2_container_name="$(service_container_name "cluster2")"

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

}

do_deploy() {
    local frontend1_ip frontend2_ip


    cli_bin=$(command -v "chef-automate")

    docker_run "${_frontend1_container_name}"
    frontend1_ip=$(container_ip "$_frontend1_container_name")
    docker_run "${_frontend2_container_name}"
    frontend2_ip=$(container_ip "$_frontend2_container_name")

    #shellcheck disable=SC2154
    docker exec -t "$_frontend1_container_name" \
        "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$test_hartifacts_path"
    docker exec -t "$_frontend2_container_name" \
        "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$test_hartifacts_path"


    #shellcheck disable=SC2154
    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" deploy config.toml \
            --hartifacts "$test_hartifacts_path" \
            --override-origin "$HAB_ORIGIN" \
            --manifest-dir "$test_manifest_path" \
            --admin-password chefautomate \
            --accept-terms-and-mlsa

    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" iam upgrade-to-v2 --beta2.1

    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" bootstrap bundle create -o bootstrap.abb

    docker exec -t "$_frontend2_container_name" \
        "$cli_bin" deploy config.toml \
            --hartifacts "$test_hartifacts_path" \
            --override-origin "$HAB_ORIGIN" \
            --manifest-dir "$test_manifest_path" \
            --admin-password chefautomate \
            --bootstrap-bundle bootstrap.abb \
            --accept-terms-and-mlsa

    start_loadbalancer $frontend1_ip $frontend2_ip
}

do_test_deploy() {
    :
}
