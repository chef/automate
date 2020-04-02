#!/bin/bash

# This is a helper function that allows use to document functions inline with the
# code, but also make that documentation available to end user in the studio.
#
# The first line of the document will show up when the user runs 'describe'.
# The full content will appear when the user runs 'describe <function>'
#
# When running in a CI environment, these documents are not created.
document() {
  name="$1"
  content=$(cat /dev/stdin)

  [[ -z "${CI:-}" ]] && echo -e "$content" > "/tmp/docs/${name}"
}

# This is a helper function that allows you to quickly and easily add an alias for
# a helper function. It will also ensure that a list of aliases are printed when
# you run `describe`.
add_alias() {
  fun="$1"
  a="$2"

  eval "alias $a=$fun"
  [[ -z "${CI:-}" ]] && echo -e "$a" > "/tmp/aliases/$fun"
}

# This function allows the user to learn more about the studio.
#
#   1. Passing no argument will print out a list of all documented functions
#   2. Passing the name of a function will print out that functions documentation
describe() {
  if [[ -z "${CI:-}" ]]; then
    echo
    if [[ -z "$1" ]]; then
      echo "The following functions are available for your use in this studio:"
      echo
      for file in /tmp/docs/*
      do
        fun="$(basename "$file")"
        echo -n "  $fun"
        [[ -f "/tmp/aliases/$fun" ]] && printf " [alias: %s]" "$(cat /tmp/aliases/"$fun")"
        echo
        echo "  $(head -n 1 "$file")"
      done
      if [[ -d "/tmp/aliases" ]]; then
        echo
        echo "ALIASES:"
        for a in /tmp/aliases/*
        do
          printf "  %-6s Alias for: %s \\n" "$(cat "$a")" "$(basename "$a")"
        done
      fi
      echo
      echo "To learn more about a particular function, run 'describe <function>'."
    else
      cat "/tmp/docs/$1"
    fi
    echo
  fi
}

getting_started() {
  [[ -z "${CI:-}" ]] && cat /dev/stdin
}

# Use `exit_with` only in very critical situations where you need to
# exit completely from the studio
exit_with() {
  error "$1"
  exit "$2"
}

document "install_if_missing" <<DOC
  Install the package if it is not installed already and binlink the binary to
  the latest installed package.
  @(arg:1) The name of the package that contains the binary (e.g., core/musl)
  @(arg:2) The name of the binary to binlink (e.g., musl-gcc)
DOC
install_if_missing() {
  if [ "$#" -ne 2 ]; then
    error "Wrong number of arguments to ${FUNCNAME[0]}"
    describe "${FUNCNAME[0]}"
    return 1
  fi

  # Install the package if it is not installed
  if [[ ! -d "/hab/pkgs/$1" ]]; then
    hab pkg install "$1" > /dev/null
  fi

  # Ensure we are binlinking to the same version `hab pkg exec` would run
  hab pkg binlink --force "$1" "$2" > /dev/null
}

document "wait_for_success" <<DOC
  Wait for the given command to succeed.
  @(arg:*) The command to run
  Environment Variable:
    TIMEOUT - The time in seconds to check until failing
  Example 1
  ---------
  wait_for_success curl localhost:8080/status
  Example 2: Wait for 60 seconds before timing out
  ---------
  TIMEOUT=60 wait_for_success curl localhost:8080/status
DOC
wait_for_success() {
  local SECONDS_WAITING=${TIMEOUT:-60}
  local COUNTER=0

  until "$@" &> /dev/null; do
    sleep 1

    log_line "Waiting for '$*' to succeed. ($(yellow "$COUNTER of $SECONDS_WAITING"))"

    if [[ $COUNTER -ge "$SECONDS_WAITING" ]]; then
      error "Command '$*' never succeeded in $(red "$SECONDS_WAITING") seconds."
      return 1
    fi
    (( COUNTER=COUNTER+1 ))
  done
}

document "wait_or_fail_for_svc_to_load" <<DOC
  Helper function to wait for a Habitat service (hab svc) to be loaded by the Habitat Supervisor.
  @(arg:1) PKG_IDENT A Habitat package identifier (ex: core/redis)
  @(arg:2) Number of seconds to wait before returning 1. (default: 60 seconds)
  @(arg:3) Wheter or not this process runs silently
DOC
wait_or_fail_for_svc_to_load() {
  local SECONDS_WAITING=${2:-60}
  local COUNTER=0

  while [[ $(hab svc status "$1" | tail -1 | awk '{print $4}') != "up" ]]; do
    sleep 1

    if [[ $COUNTER -ge "$SECONDS_WAITING" ]]; then
      error "Habitat service '$(red "$1")' failed to load."
      return 1
    fi

    (( COUNTER=COUNTER+1 ))

    [[ "${3:-}" != "silent" ]] && log_line "Waiting to load svc '$(yellow "$1")'. ($(yellow "$COUNTER of $SECONDS_WAITING"))";
  done
}

document "wait_or_fail_for_port_to_listen" <<DOC
  Wait for a port to be listening. If the port is not found in the set amount of time this helper returns 1.
  @(arg:1) Port to wait for to be listening
  @(arg:2) Number of seconds to wait before returning 1. (default: 60 seconds)
  @(arg:3) Wheter or not this process runs silently
  Example: Wait for a service to start listening on port 1234 for 1 minute (defaults)
  ---------------------------------------------
  wait_or_fail_for_port_to_listen 1234
  Example: Wait for a service to start listening on port 1234 for 10 seconds
  ---------------------------------------------
  wait_or_fail_for_port_to_listen 1234 10
DOC
wait_or_fail_for_port_to_listen() {
  install_if_missing core/busybox-static netstat
  local SECONDS_WAITING=${2:-60}

  local COUNTER=0
  while ! netstat -an | grep "$1" | grep LISTEN &>/dev/null; do
    sleep 1

    if [[ $COUNTER -ge "$SECONDS_WAITING" ]]; then
      error "Failed listening for port '$1'"
      return 1
    fi

    (( COUNTER=COUNTER+1 ))
    [[ "${3:-}" != "silent" ]] && log_line "Waiting for port '$(yellow "$1")' to be listening ($(yellow "$COUNTER of $SECONDS_WAITING"))";
  done
}

document "wait_for_ok_response" <<DOC
  Wait a set amount of time for the provided URL to response with status code (200).
  If the time expires then this helper returns 1
  @(arg:1) URL to curl
  @(arg:2) Number of seconds to wait before returning 1. (default: 60 seconds)
  Example: Wait for www.google.com to return Ok for 1 minute
  ---------------------------------------------
  wait_or_fail_for_ok_response www.google.com 60
  Example: Wait for www.google.com to return Ok for 10 seconds
  ---------------------------------------------
  wait_or_fail_for_ok_response www.google.com 10
DOC
wait_for_ok_response() {
  local url="${1:?Missing url argument; try 'describe wait_for_ok_response'}"
  local SECONDS_TO_WAIT=${2:-60}

  code=200;
  install_if_missing core/curl curl
  local COUNTER=0
  local response

  while [[ $COUNTER -le "$SECONDS_TO_WAIT"  ]]; do
    # shellcheck disable=SC1083
    response="$(curl --write-out %{http_code} --silent --output /dev/null "$url")"
    if [[ $response -eq $code ]]; then
      break
    else
      log_line "Waiting for '$url' to response OK ($code). [Got:$response] ($(yellow "$COUNTER of $SECONDS_TO_WAIT"))"
      sleep 1
      (( COUNTER=COUNTER+1 ))
    fi

    if [[ $COUNTER -ge "$SECONDS_WAITING" ]]; then
      return 1
    fi
  done
}

document "install_go_tool" <<DOC
  Install the specified go tool(s).
  @(arg:*) The array of packages you wish to install.
  The default behavior is to install go tools with 'go install -v -mod=vendor'

  To install tools using not included in the vendor you'll either need to
  include them and revendor or unset GO_LDFLAGS so that we don't use the
  module vendor directory when installing the binary to the GOBIN directory.
DOC
install_go_tool() {
  install_if_missing "$(desired_golang_ident)" go
  for tool in "$@"; do
    go_tool=$(basename "$tool")
    if [[ ! -f "${GOBIN}/${go_tool}" ]]; then
      eval "go install -v $tool"
    fi
  done
}

document "compile_go_protobuf" <<DOC
  Compile the protobuf definitions from the provided script.
  @(arg:1) Path to the script to execute. (default: scripts/grpc.sh)
  The script should look like:
  ------------------------------------------------------------------
  #!/bin/bash
  set -x
  GOPATH=\$(go env GOPATH)
  protoc -I. \\
    -I\$GOPATH/src \\
    --go_out=plugins=grpc:. \\
    proto/*.proto
  ------------------------------------------------------------------
DOC
compile_go_protobuf() {
  local rc
  proto_script="${1:-scripts/grpc.sh}"

  # Verify that the script exist and it is an executable
  if [[ -x $proto_script ]]; then
    install_if_missing core/protobuf-cpp protoc
    install_go_tool github.com/golang/protobuf/protoc-gen-go

    # Install grpc-gateway
    # only_if the script has an entry like: '--grpc-gateway_out'
    if [[ "$(grep -w "\\-\\-grpc-gateway_out" "$proto_script")" != "" ]]; then
      local grpc_gateway_proto_tools=(
        github.com/grpc-ecosystem/grpc-gateway/protoc-gen-grpc-gateway
        github.com/grpc-ecosystem/grpc-gateway/protoc-gen-swagger
      )

      install_go_tool "${grpc_gateway_proto_tools[@]}"
    fi

    # Install protoc-gen-lint from the internet instead of
    # installing it from our vendor/ directory. (why? because if
    # not we will need to add it to all our Gopkg.toml files)
    #
    # only_if the script has an entry like: '--lint_out'
    if [[ "$(grep -w "\\-\\-lint_out" "$proto_script")" != "" ]]; then
      install_go_tool github.com/ckaznocha/protoc-gen-lint
    fi

    eval "$proto_script";
    rc=$?
  else
    error "File '$proto_script' doesn't exist or is not executable."
    error "Try 'describe ${FUNCNAME[0]}'."
    return 1
  fi

  return $rc
}
