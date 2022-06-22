#!/bin/bash

A2_WORK_DIR="/go/src/github.com/chef/automate"
SERVICES_CONFIG_PATH=""

create_services_config_path() {
    if [ -z "$SERVICES_CONFIG_PATH" ]; then
        log_info "Creating services config path"
        SERVICES_CONFIG_PATH=$(mktemp -d)
    fi
}

destroy_services_config_path() {
    if [ -n "$SERVICES_CONFIG_PATH" ]; then
        log_info "Removing services config path $SERVICES_CONFIG_PATH"
        sudo rm -rf "$SERVICES_CONFIG_PATH"
    fi
}

docker_run() {
    local name="$1"
    local image="$2"
    local volume_mount=""
    
    if [ -z "$image" ]; then
        image="chefes/a2-integration:latest"
    fi

    if [[ -n $HOST_PWD ]]; then
        echo "Updating HOST_PWD to have :"
        volume_mount="${HOST_PWD}:"
    fi
    local docker_run_args=(
            "--detach"
            "--env" "HOST_PWD"
            "--env" "HAB_ORIGIN=$HAB_ORIGIN"
            "--env" "HAB_STUDIO_SUP=false"
            "--env" "HAB_NONINTERACTIVE=true"
            "--env" "CONTAINER_HOSTNAME=$name"
            "--hostname" "$name"
            "--interactive"
            "--name" "$name"
            "--privileged"
            "--rm"
            "--tmpfs=/tmp:rw,noexec,nosuid"
            "--tmpfs=/var/tmp:rw,noexec,nosuid"
            "--tmpfs=/dev/shm:rw,noexec,nosuid"
            "--tty"
            "--volume" "${volume_mount}${A2_WORK_DIR}"
            "--workdir" "$A2_WORK_DIR"
    )

    if [ -n "$SERVICES_CONFIG_PATH" ]; then
    docker_run_args+=(
            "--volume" "$SERVICES_CONFIG_PATH:/services"
    )
    fi

    if [ "$CI" == "true" ]; then
        buildkite_agent=$(command -v buildkite-agent)
        docker_run_args+=(
            "--env" "BUILDKITE_JOB_ID"
            "--env" "BUILDKITE_BUILD_ID"
            "--env" "BUILDKITE_AGENT_ACCESS_TOKEN"
            "--env" "BUILDKITE"
            "--volume" "$buildkite_agent:/usr/bin/buildkite-agent"
            "--label" "buildkitejob=$BUILDKITE_JOB_ID"
        )
    fi

    echo "${docker_run_args[*]}"

    docker run "${docker_run_args[@]}" "$image"
}

service_config_path() {
    echo "$SERVICES_CONFIG_PATH"/"$1" 
}

service_container_name() {
    #shellcheck disable=SC2154
    echo "${1}-${test_build_slug}"
}

container_ip() {
    docker inspect --format '{{ .NetworkSettings.IPAddress }}' "$@"
}
