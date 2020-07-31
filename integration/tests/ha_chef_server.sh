#!/bin/bash

#shellcheck disable=SC2034
test_name="ha_chef_server"
test_external_services=(ha_backend)

source integration/services/common.sh

_frontend1_container_name="$(service_container_name "cs1")"
_frontend2_container_name="$(service_container_name "cs2")"

do_setup() {
    do_setup_default

    echo "Setting up Habitat"
    groupadd hab
    useradd -g hab hab

    echo "Making /hab/svc"
    mkdir -p /hab/svc

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
    local frontend1_ip frontend2_ip

    cli_bin=$(command -v "chef-automate")

    hab pkg install --channel="$test_channel" --binlink chef/automate-cs-nginx

    docker_run "${_frontend1_container_name}"
    docker_run "${_frontend2_container_name}"
    #shellcheck disable=SC2154
    docker exec -t "$_frontend1_container_name" \
        "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$test_hartifacts_path"
    docker exec -t "$_frontend2_container_name" \
        "$(a2_root_dir)/scripts/copy_hartifacts.sh" "$test_hartifacts_path"

    frontend1_ip=$(container_ip "$_frontend1_container_name")
    frontend2_ip=$(container_ip "$_frontend2_container_name")

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

    "$cli_bin" bootstrap bundle unpack bootstrap.abb

    start_loadbalancer "$frontend1_ip" "$frontend2_ip"
}

do_test_deploy() {
    cat << EOH > /tmp/pivotal.rb
log_location     STDOUT
node_name        'pivotal'
chef_server_url  'https://localhost'
chef_server_root 'https://localhost'
ssl_verify_mode  :verify_none
client_key       '/hab/svc/automate-cs-oc-erchef/data/pivotal.pem'
EOH
    chef-server-ctl test -c /tmp/pivotal.rb
}

do_cleanup() {
    docker stop "$_frontend1_container_name"
    docker stop "$_frontend2_container_name"
}
