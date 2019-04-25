#!/bin/bash

AUTOMATE_CLUSTER_DIR=$(dirname ${BASH_SOURCE[0]})

automate_cluster_container1=$(service_container_name "automate_cluster_1")
automate_cluster_container2=$(service_container_name "automate_cluster_2")
automate_cluster_container_nginx=$(service_container_name "automate_cluster_nginx")

automate_cluster_private=$(service_config_path "automate_cluster_private")

automate_cluster_setup() {
    echo "Automate cluster directory: $automate_cluster_private"

    mkdir -p $automate_cluster_private

    curl https://packages.chef.io/files/dev/automate/latest/chef-automate_linux_amd64.zip | gunzip - > \
        "$automate_cluster_private/chef-automate" && chmod +x "$automate_cluster_private/chef-automate"

    docker_run $automate_cluster_container1
    local automate_cluster_container1_ip=$(container_ip $automate_cluster_container1)
    log_info "Launched $automate_cluster_container1 with ip ${automate_cluster_container1_ip}"

    docker_run $automate_cluster_container2
    local automate_cluster_container2_ip=$(container_ip $automate_cluster_container2)
    log_info "Launched $automate_cluster_container2 with ip ${automate_cluster_container2_ip}"

    #hab pkg exec core/openssl openssl req -x509 -nodes -days 3650 -newkey rsa:2048 \
        #-keyout $automate_cluster_private/nginx-selfsigned.pem \
        #-out $automate_cluster_private/nginx-selfsigned.crt \
        #-reqexts SAN -extensions SAN -subj '/CN=elasticsearch' \
        #-config \
        #<(cat "$openssl_cnf" \
        #<(printf "\n[SAN]\nsubjectAltName=IP.1:${automate_cluster_container1_ip},IP.2:${automate_cluster_container2_ip}"))

    docker cp "$AUTOMATE_CLUSTER_DIR/a2_setup.sh" "${automate_cluster_container1}:/a2_setup.sh"
    docker cp "$AUTOMATE_CLUSTER_DIR/a2_setup.sh" "${automate_cluster_container2}:/a2_setup.sh"
    #docker cp "$AUTOMATE_CLUSTER_DIR/nginx_setup.sh" "${automate_cluster_container_nginx}:/nginx_setup.sh"

    docker exec -t $automate_cluster_container1 /a2_setup.sh "$automate_cluster_container1_ip"
    docker exec -t $automate_cluster_container2 /a2_setup.sh "$automate_cluster_container2_ip"
    #docker exec -t $automate_cluster_container_nginx /nginx_setup.sh "$automate_cluster_container_nginx_ip"
}

automate_cluster_dump_logs() {
    :
}

automate_cluster_teardown() {
    docker stop "$automate_cluster_container1"
    docker stop "$automate_cluster_container2"
    docker stop "$automate_cluster_container_nginx"
}
