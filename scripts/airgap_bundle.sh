#!/bin/bash

# Set -x to enable debugging

set -eEuo pipefail

export AUTOMATE_BIN_URL="https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip"
export BANNER="
This script takes a manifest.json and outputs a tarball of
all packages therein including their transitive dependencies. It accomplishes
this by wrapping the 'chef-automate' cli utility.

The following arguments are available:
 [REQUIRED]
 -t [frontend|backend]            Generate an Air Gap Bundle for either frontend or backend.
 -o [output tarball]              Output file for archive (.tar.gz)
 -m [manifest]                    The path to a manifest.json file [required for '-t backend' - cannot be used with '-c']
 -c [channel]                     The channel to query for latest hartifacts [required for '-t frontend' - cannot be used with '-m']

 [OPTIONS]
 -d [download path]               Download path for chef-automate binary (default: /tmp/chef-automate)
 -w [workspace]                   Workspace where packages will be downloaded (default: /tmp/workspace)
 -h                               Print this help message

 ex. $0 -t backend -m /path/to/manifest.json -o /tmp/bundle.tar.gz
 ex. $0 -t frontend -c current -o /tmp/bundle.tar.gz -o /tmp/bundle.tar.gz

"
export DOCKER_IMAGE="chefes/lita-worker"
# Some sane defaults
export CHEF_AUTOMATE_BIN_PATH="/tmp/chef-automate"
export WORKSPACE_PATH="/tmp/workspace"
# These are required args so we ensure they are not given a default value
export BUNDLE_TYPE=
export TARBALL_PATH=
export MANIFEST_PATH=
export CHANNEL=

TEMP_DIR=/tmp
export TEMP_BUNDLE_FILE=$TEMP_DIR/bundle.aib.$$.$RANDOM
export TEMP_TAR_FILE=$TEMP_DIR/my.aib.$$.$RANDOM

# Helper Functions
echo_env() {
	echo "=============================================="
	echo "Bundle Type:               ${BUNDLE_TYPE}"
	echo "Script Path:               ${ABSOLUTE_PATH}"
	echo "Repo Path:                 ${REPO_PATH}"
	echo "Manifest:                  ${MANIFEST_PATH}"
	echo "Channel:                   ${CHANNEL}"
	echo "Output tarball:            ${TARBALL_PATH}"
	echo "Download chef-automate to: ${CHEF_AUTOMATE_BIN_PATH}"
	echo "Workspace:                 ${WORKSPACE_PATH}"
	echo "=============================================="
}

download_automate_cli() {
  hab pkg exec core/curl curl -s "${AUTOMATE_BIN_URL}" | gunzip - > "${CHEF_AUTOMATE_BIN_PATH}" && chmod +x "${CHEF_AUTOMATE_BIN_PATH}"
}

usage() {
  echo "${BANNER}"
  exit 1
}

abs_path(){
  # this function is responsible for determining the
  # absolute path of $1 (a file) on the filesystem
  # https://stackoverflow.com/a/13087801
  if [[ -d "$1" ]]; then
    pushd "$1" >/dev/null
    pwd
    popd >/dev/null
  elif [[ -e $1 ]]; then
    pushd "$(dirname "$1")" >/dev/null
    echo "$(pwd)/$(basename "$1")"
    popd >/dev/null
  else
    echo "$1" does not exist! >&2
    return 127
  fi
}

is_linux() {
  if uname -s | grep -q Linux; then
    return 0
  else
    return 1
  fi
}

clean_up() {
  rm -f "${TEMP_BUNDLE_FILE}"
  rm -f "${TEMP_TAR_FILE}"
  exit "${1:-0}"
}

trap clean_up SIGHUP SIGINT SIGTERM ERR

airgap_bundle_create() {
  original_aib_path="${TEMP_BUNDLE_FILE}"
  args=('airgap' 'bundle' 'create')
  if [[ "${BUNDLE_TYPE}" == "frontend" ]]; then
    args+=('-c' "${CHANNEL}")
  else
    args+=('-m' "${MANIFEST_PATH}")
  fi
  args+=("${original_aib_path}")
  # printf '%s\n' "Running: ${CHEF_AUTOMATE_BIN_PATH} ${args[*]}"
  if "${CHEF_AUTOMATE_BIN_PATH}" "${args[@]}" > /tmp/thelog.log; then
    if [[ "${BUNDLE_TYPE}" == "backend" ]]; then
      # this removes the magic header from the .aib
      # making it usable with the tar command
      # https://github.com/chef/a2/blob/4c51540f822d7ddcb32d192bc7dbf33803789a8e/components/automate-deployment/pkg/airgap/bundle_creator.go#L3
      tail -c +8 "${original_aib_path}" > "${TEMP_TAR_FILE}" && cat "${TEMP_TAR_FILE}" > "${TARBALL_PATH}"
      rm -f ${TEMP_TAR_FILE}
    else
      cat "${original_aib_path}" > "${TARBALL_PATH}"
    fi
    rm -f "${original_aib_path}"
    outfile=${ORIGINAL_TARBALL:-${TARBALL_PATH}}
    # echo "Airgap Bundle: ${outfile}"
    bname=$(basename "${outfile}")
    if [[ "${BUNDLE_TYPE}" == "frontend" ]]; then
      echo "frontend_aib_dest_file = \"/var/tmp/${bname}\""
      echo "frontend_aib_local_file = \"${bname}\""
    else
      echo "backend_aib_dest_file = \"/var/tmp/${bname}\""
      echo "backend_aib_local_file = \"${bname}\""
    fi
  else
    echo "✘ ERROR"
    cat /tmp/thelog.log
    exit 1
  fi
}

exec_linux() {
  download_automate_cli
  airgap_bundle_create
}

exec_docker() {
  # in Docker mode we hard code various arguments
  cmd="/workspace/scripts/$(basename "$0")"
  args=('-t' "${BUNDLE_TYPE}" '-d' '/tmp/chef-automate' '-w' '/tmp/archive' '-o' '/tmp/bundle.tar')
  if [[ "${BUNDLE_TYPE}" == "frontend" ]]; then
    args+=('-c' "${CHANNEL}")
  else
    args+=('-m' '/workspace/manifest.json')
  fi
  touch "${TARBALL_PATH}"
  docker run --rm -it \
    --env="ORIGINAL_TARBALL=${TARBALL_PATH}" \
    --volume "${REPO_PATH}":/workspace:ro \
    --volume "${TARBALL_PATH}":/tmp/bundle.tar:rw \
    --cap-drop="NET_BIND_SERVICE" \
    --cap-drop="SETUID" \
    --cap-drop="SETGID" \
    -d=false \
    ${DOCKER_IMAGE} \
    "${cmd}" "${args[@]}"
}

do_tasks() {
  # echo_env
  if is_linux; then
    exec_linux
  else
    exec_docker
  fi
}

# End Helper Functions

# CLI Option Parsing

if [ $# -eq 0 ]; then
  usage
fi

while getopts ":m:d:t:c:w:o:h" opt; do
  case "${opt}" in
    m)
      export MANIFEST_PATH=${OPTARG}
      ;;
    d)
      export CHEF_AUTOMATE_BIN_PATH=${OPTARG}
      ;;
    c)
      export CHANNEL=${OPTARG}
      ;;
    w)
      export WORKSPACE_PATH=${OPTARG}
      ;;
    o)
      export TARBALL_PATH=${OPTARG}
      ;;
    t)
      export BUNDLE_TYPE=${OPTARG}
      ;;
    h)
      usage
      ;;
    \?)
      echo "Invalid option: -${OPTARG}" >&2
      exit 1
      ;;
    :)
      echo "Option -${OPTARG} requires an argument." >&2
      exit 1
      ;;
  esac
done
shift $((OPTIND -1))

# Additional validations
if [[ -z ${BUNDLE_TYPE} ]]; then
  echo "ERROR: required option -t not specified!"
  usage
fi
if [[ -z ${TARBALL_PATH} ]]; then
  echo "ERROR: required option -o not specified!"
  usage
fi
if [[ -n ${CHANNEL} && -n ${MANIFEST_PATH} ]]; then
  echo "ERROR: options -m and -c are mutually exclusive!"
  usage
fi
if [[ -n ${MANIFEST_PATH} && ! -f "${MANIFEST_PATH}" ]]; then
  echo "ERROR: Manifest does not exist: ${MANIFEST_PATH}"
  exit 1
fi

case "$BUNDLE_TYPE" in
  frontend)
    if [[ -z ${CHANNEL} ]]; then
      export CHANNEL="current"
    fi
    ;;
  backend)
    if [[ -z ${MANIFEST_PATH} ]]; then
      echo "ERROR: -t backend requires option -m"
      usage
    fi
    ;;
  *)
    usage
    ;;
esac

# End CLI Option Parsing

# Set up some path context variables

ABSOLUTE_PATH=$(abs_path "${0}")
export ABSOLUTE_PATH
REPO_PATH=$(dirname "$(dirname "${ABSOLUTE_PATH}")")
export REPO_PATH

do_tasks
clean_up "$@"
