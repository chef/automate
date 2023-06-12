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
    #shellcheck disable=SC2154
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
            --enable-chef-server  \
            --hartifacts "$test_hartifacts_path" \
            --override-origin "$HAB_ORIGIN" \
            --manifest-dir "$test_manifest_path" \
            --admin-password chefautomate \
            --accept-terms-and-mlsa

    docker exec -t "$_frontend1_container_name" \
        "$cli_bin" bootstrap bundle create -o bootstrap.abb

    docker exec -t "$_frontend2_container_name" \
        "$cli_bin" deploy config.toml \
            --enable-chef-server \
            --hartifacts "$test_hartifacts_path" \
            --override-origin "$HAB_ORIGIN" \
            --manifest-dir "$test_manifest_path" \
            --admin-password chefautomate \
            --bootstrap-bundle bootstrap.abb \
            --accept-terms-and-mlsa

    "$cli_bin" bootstrap bundle unpack bootstrap.abb

    start_loadbalancer "$frontend1_ip" "$frontend2_ip"

    mkdir -p /hab/svc/automate-cs-oc-erchef/config
    cat << EOH > /hab/svc/automate-cs-oc-erchef/config/pedant_config.rb
chef_server_uid = "chef-server_#{Process.pid}".downcase

org({:name => "pedant_testorg_#{chef_server_uid}",
     :create_me => true})

# You MUST specify the address of the server the API requests will be
# sent to.  Only specify protocol, hostname, and port.
chef_server "https://localhost:443"
base_resource_url "https://localhost:443"
explicit_port_url true

# SSL protocol version to use for secure communications
# with the load balancer
ssl_version :TLSv1_2
ssl_verify_mode  :verify_none

# Test users.  The five users specified below are required; their
# names (:user, :non_org_user, etc.) are indicative of their role
# within the tests.  All users must have a ':name' key.  If they have
# a ':create_me' key, Pedant will create these users for you.  If you
# are using pre-existing users, you must supply a ':key_file' key,
# which should be the fully-qualified path /on the machine Pedant is
# running on/ to a private key for that user.

superuser_name 'pivotal'
superuser_key  '/hab/svc/automate-cs-oc-erchef/data/pivotal.pem'
webui_key '/hab/svc/automate-cs-oc-erchef/data/webui_priv.pem'
stats_user 'statsuser'

suite "api"

requestors({
             :clients => {
               :admin => {
                 :name => "pedant_admin_client_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :admin => true
               },
               :non_admin => {
                 :name => "pedant_client_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
               },
               :bad => {
                 :name => "bad_client_#{chef_server_uid}",
                 :create_me => true,
                 :bogus => true
               }
             },

             :users => {
               # An administrator in the testing organization
               :admin => {
                 :name => "pedant_admin_user_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :admin => true
               },

               :non_admin => {
                 :name => "pedant_user_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :admin => false
               },

               # A user that is not a member of the testing organization
               :bad => {
                 :name => "pedant_nobody_#{chef_server_uid}",
                 :create_me => true,
                 :create_knife => true,
                 :associate => false
               },
             }
           })

# Default server api version for all requests that don't specify it.
server_api_version         0

# Enable/Disable tests if the required_recipe endpoint is turned on
required_recipe_enabled false

reindex_endpoint "https://127.0.0.1"
internal_server "https://$frontend1_ip:10203"
EOH
}

do_test_deploy() {
    ## skipping status test because of the missing file in automate - /etc/opscode/chef-server-running.json 
    ## adding smoke tag or else all the test will be considered skipping only the status test
    hab pkg exec chef/automate-cs-nginx chef-server-ctl test --smoke --skip-status
}

do_cleanup() {
    docker stop "$_frontend1_container_name"
    docker stop "$_frontend2_container_name"
}
