#!/bin/bash

SERVICE_A_DIR=$(dirname "${BASH_SOURCE[0]}")

service_a_container1=$(service_container_name "service_a_1")
service_a_container2=$(service_container_name "service_a_2")
service_a_config=$(service_config_path "service_a")

service_a_setup() {
    mkdir -p "$(dirname "$service_a_config")"
    docker_run "$service_a_container1"
    docker cp "$SERVICE_A_DIR/setup.sh" "${service_a_container1}:/setup.sh"
    docker exec "$service_a_container1" /setup.sh
    log_info "Launched $service_a_container1 with ip $(container_ip "$service_a_container1")"
    docker_run "$service_a_container2"
    log_info "Launched $service_a_container2 with ip $(container_ip "$service_a_container2")"
    echo "hello" > "$service_a_config"
}

service_a_dump_logs() {
    :
}

service_a_teardown() {
    docker stop "$service_a_container1"
    docker stop "$service_a_container2"
}
