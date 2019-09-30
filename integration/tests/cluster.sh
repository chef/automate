#!/bin/bash

#shellcheck disable=SC2034
test_name="cluster"
test_external_services=(ha_backend)

source integration/services/common.sh

_frontend1_container_name="$(service_container_name "cluster1")"
_frontend2_container_name="$(service_container_name "cluster2")"
_ssh_node_container_name="$(service_container_name "ssh_node")"

do_setup() {
    do_setup_default

    echo "Installing docker"
    hab pkg install --binlink core/docker
    echo "Installed docker"
    echo "Installing inspec"
    # inspec 4.17.7 is broken
    hab pkg install --binlink chef/inspec/4.16.0/20190829191134
    echo "Installed inspec"
}

do_create_config() {
    do_create_config_default
    #shellcheck disable=SC2154
    cat /services/ha_backend.toml >> "$test_config_path"

}

start_ssh_node() {
    docker_run "${_ssh_node_container_name}" chefes/ssh-target-ubuntu1804:latest
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
        "$cli_bin" iam upgrade-to-v2

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

    start_loadbalancer "$frontend1_ip" "$frontend2_ip"

    start_ssh_node
}


do_test_deploy() {
    local test_container_ip frontend1_ip frontend2_ip

    #shellcheck disable=SC2154
    test_container_ip=$(container_ip "$test_container_name")
    frontend1_ip=$(container_ip "$_frontend1_container_name")
    frontend2_ip=$(container_ip "$_frontend2_container_name")

    export ELASTICSEARCH_URL="http://$frontend1_ip:10144"
    test_notifications_endpoint="http://$test_container_ip:15555"

    run_inspec_tests "${A2_ROOT_DIR}" "a2-iam-v2-integration"

    local admin_token
    admin_token=$(docker exec -t "$_frontend1_container_name" \
        "$cli_bin" iam token create --admin "diagnostics-test-$RANDOM")

    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" diagnostics run --admin-token "$admin_token" "~iam-v1"


    docker exec -t "$_frontend2_container_name" \
        "$cli_bin" diagnostics run --admin-token "$admin_token" "~iam-v1"

    "$cli_bin" diagnostics run --admin-token "$admin_token" "~iam-v1" "~purge" "~cli" "~grpc" "~deployment"
}
