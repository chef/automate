#!/bin/bash

main() {
  token=""

  # Parse command line flags and options.
  while getopts ":ht:" opt; do
    case "${opt}" in
      h)
        print_help
        exit 0
        ;;
      t)
        token="${OPTARG}"
        ;;
      \?)
        echo "" >&2
        print_help >&2
        exit 1
        ;;
    esac
  done

  ensure_container_build || exit 1
  run_hab_in_container || exit 1
}

print_help() {
  local _cmd
  _cmd="$(basename "${0}")"
  cat <<USAGE
${_cmd}

Run a hab sup, configured to emit data to your local Chef Automate, in a docker
container

USAGE:
    ${_cmd} [FLAGS]

FLAGS:
    -h    Prints help information
    -t    Chef Automate API token

USAGE
}

ensure_container_build() {
  # For initial implementation, just run build all the time. All the layers get
  # cached and it will be fast the second and subsequent times you run it.
  # Downside of this approach is we are not being smart about the caching and
  # folks will have to know when/how to clean the cache and force a new image
  # build.
  # FYI, the way to nuke the image:
  # docker rmi -f chef/automate-apps-hab-client
  docker build . -t chef/automate-apps-hab-client
}

run_hab_in_container() {
  docker run -it chef/automate-apps-hab-client bash -c "
    export HAB_FEAT_EVENT_STREAM=1 && \
    hab sup run \
      --event-stream-application=MY_APP \
      --event-stream-environment=MY_ENV \
      --event-stream-site=MY_SITE \
      --event-stream-url=host.docker.internal:4222 \
      --event-stream-token=$token &

    # wait for hab to come up
    while ! hab svc status > /dev/null 2>&1; do
        sleep 1;
    done
    
    hab svc load core/redis
    hab svc load core/nginx

    # wait for things to come up and send some messages
    sleep 60
  "
    
}

main "$@" || exit 99
