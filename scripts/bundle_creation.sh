#!/bin/bash
# Set -x to enable debugging
set -eEuo pipefail
export BANNER="
This script takes a manifest.json and outputs a tarball of
all packages therein including their transitive dependencies. It accomplishes
this by wrapping the 'chef-automate' cli utility.
The following arguments are available:
 [REQUIRED]
 -t [frontend|backend|all]            Generate an Air Gap Bundle for either frontend or backend or both
 -o [output tarball]              Output file for archive (.tar.gz)
 [OPTIONS]
 -h                               Print this help message
 ex. $0 -t upgradebackends  -o /tmp/bundle.tar.gz
"
export DOCKER_IMAGE="chefes/lita-worker"
# Some sane defaults
export CHEF_AUTOMATE_BIN_PATH="/usr/bin/chef-automate"
export WORKSPACE_PATH="/tmp/workspace"
export HAB_LICENSE=accept-no-persist
# These are required args so we ensure they are not given a default value
export BUNDLE_TYPE=
export BACKENDAIB_TFVARS=
export FRONTENDAIB_TFVARS=
export TARBALL_PATH=
export CHANNEL=current
TEMP_DIR=/tmp
export TEMP_BUNDLE_FILE=$TEMP_DIR/bundle.aib.$$.$RANDOM
export TEMP_TAR_FILE=$TEMP_DIR/my.aib.$$.$RANDOM
export MANIFEST_TFVARS="terraform/a2ha_manifest.auto.tfvars"
export BACKENDAIB=
export PACKAGES_INFO="/tmp/packages.info"

# Helper Functions
echo_env() {
    echo "=============================================="
    echo "Bundle Type:               ${BUNDLE_TYPE}"
    echo "Script Path:               ${ABSOLUTE_PATH}"
    echo "Repo Path:                 ${REPO_PATH}"
    echo "Output tarball:            ${TARBALL_PATH}"
    echo "Workspace:                 ${WORKSPACE_PATH}"
    echo "Backend Tarball PAth:      ${BACKENDAIB}"
    echo "=============================================="
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
  args+=('-c' "${CHANNEL}")
  args+=("${original_aib_path}")
  # printf '%s\n' "Running: ${CHEF_AUTOMATE_BIN_PATH} ${args[*]}"
  if "${CHEF_AUTOMATE_BIN_PATH}" "${args[@]}" > /tmp/thelog.log; then
    if [ "$BUNDLE_TYPE" == "upgradefrontends" ] || [ "$BUNDLE_TYPE" == "all" ]
    then
          cat "${original_aib_path}" > "${TARBALL_PATH}"
          outfile=${ORIGINAL_TARBALL:-${TARBALL_PATH}}
          bname=$(basename "${outfile}")
          echo "frontend_aib_dest_file = \"/var/tmp/${bname}\""  > "${FRONTENDAIB_TFVARS}"
          echo "frontend_aib_local_file = \"${bname}\"" >> "${FRONTENDAIB_TFVARS}"
    fi

    if [ "$BUNDLE_TYPE" == "upgradebackends" ] || [ "$BUNDLE_TYPE" == "all" ]
    then
          # getting packges info from airgap bundle       
          ${CHEF_AUTOMATE_BIN_PATH}  airgap bundle info ${original_aib_path} > ${PACKAGES_INFO}
          tail -c +8 "${original_aib_path}" > "${TEMP_TAR_FILE}" && cat "${TEMP_TAR_FILE}" > "${BACKENDAIB}"
          # this removes the magic header from the .aib
          # making it usable with the tar command
          rm -f ${TEMP_TAR_FILE}    
          outfile_backend=${ORIGINAL_TARBALL:-${BACKENDAIB}}
          backend_name=$(basename "${outfile_backend}")
          echo "backend_aib_dest_file = \"/var/tmp/${backend_name}\"" > "${BACKENDAIB_TFVARS}" 
          echo "backend_aib_local_file = \"${backend_name}\"" >> "${BACKENDAIB_TFVARS}"
    fi
    rm -f "${original_aib_path}"
  else
    echo "✘ ERROR"
    cat /tmp/thelog.log
    exit 1
  fi
  
    #Create Manifest auto_tfvars
    if [ "$BUNDLE_TYPE" != "upgradefrontends" ]
    then
       create_manifest_auto_tfvars
    fi

}

exec_linux() {
  airgap_bundle_create
}

# We are creating a2ha_manifest.auto.tfvars as they will be used by terraform modules while deployment
create_manifest_auto_tfvars(){
  cat >"${MANIFEST_TFVARS}" <<EOL
  pgleaderchk_pkg_ident = " $(grep "automate-ha-pgleaderchk" ${PACKAGES_INFO})"
  postgresql_pkg_ident = " $(grep "automate-ha-postgresql" ${PACKAGES_INFO})" 
  proxy_pkg_ident = " $(grep "automate-ha-haproxy" ${PACKAGES_INFO})"
  journalbeat_pkg_ident = " $(grep "automate-ha-journalbeat" ${PACKAGES_INFO})"
  metricbeat_pkg_ident = " $(grep "automate-ha-metricbeat" ${PACKAGES_INFO})"
  kibana_pkg_ident = " $(grep "automate-ha-kibana" ${PACKAGES_INFO})"
  elasticsearch_pkg_ident = " $(grep "automate-ha-elasticsearch" ${PACKAGES_INFO})"
  elasticsidecar_pkg_ident = " $(grep "automate-ha-elasticsidecar" ${PACKAGES_INFO})"
  curator_pkg_ident = " $(grep "automate-ha-curator" ${PACKAGES_INFO})"
EOL
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
while getopts ":b:t:w:o:h:v:q:c:" opt; do
  case "${opt}" in
    w)
      export WORKSPACE_PATH=${OPTARG}
      ;;
    o)
      export TARBALL_PATH=${OPTARG}
      ;;
    b)
      export BACKENDAIB=${OPTARG}
      ;;  
    t)
      export BUNDLE_TYPE=${OPTARG}
      ;;
    v)
      export FRONTENDAIB_TFVARS=${OPTARG}
      ;;
    q)
      export BACKENDAIB_TFVARS=${OPTARG}
      ;;
    c)
      export CHANNEL=${OPTARG}
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
ABSOLUTE_PATH=$(abs_path "${0}")
export ABSOLUTE_PATH
REPO_PATH=$(dirname "$(dirname "${ABSOLUTE_PATH}")")
export REPO_PATH
do_tasks
clean_up "$@"
