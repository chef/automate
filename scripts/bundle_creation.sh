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
 -t [frontend]            Generate an Air Gap Bundle for either frontend.
 -o [output tarball]              Output file for archive (.tar.gz)
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
export BACKENDAIB_TFVARS=
export FRONTENDAIB_TFVARS=
export TARBALL_PATH=
export MANIFEST_PATH="manifest.json"
TEMP_DIR=/tmp
export TEMP_BUNDLE_FILE=$TEMP_DIR/bundle.aib.$$.$RANDOM
export TEMP_TAR_FILE=$TEMP_DIR/my.aib.$$.$RANDOM
export MANIFEST_TFVARS="terraform/a2ha_manifest.auto.tfvars"
export BACKENDAIB=
# Helper Functions
echo_env() {
    echo "=============================================="
    echo "Bundle Type:               ${BUNDLE_TYPE}"
    echo "Script Path:               ${ABSOLUTE_PATH}"
    echo "Repo Path:                 ${REPO_PATH}"
    echo "Output tarball:            ${TARBALL_PATH}"
    echo "Download chef-automate to: ${CHEF_AUTOMATE_BIN_PATH}"
    echo "Workspace:                 ${WORKSPACE_PATH}"
    echo "Backend Tarball PAth:      ${BACKENDAIB}"
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
      tail -c +8 "${original_aib_path}" > "${TEMP_TAR_FILE}" && cat "${TEMP_TAR_FILE}" > ${TARBALL_PATH}
      rm -f ${TEMP_TAR_FILE}
    else
      cat "${original_aib_path}" > ${TARBALL_PATH}
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

new_airgap_bundle_create() {
  original_aib_path="${TEMP_BUNDLE_FILE}"
  args=('airgap' 'bundle' 'create')
  args+=("${original_aib_path}")
  # printf '%s\n' "Running: ${CHEF_AUTOMATE_BIN_PATH} ${args[*]}"
  if "${CHEF_AUTOMATE_BIN_PATH}" "${args[@]}" > /tmp/thelog.log; then
    if [ "$BUNDLE_TYPE" == "upgradefrontends" ] || [ "$BUNDLE_TYPE" == "all" ]
    then
          cat "${original_aib_path}" > ${TARBALL_PATH}
    fi

    if [ "$BUNDLE_TYPE" == "upgradebackends" ] || [ "$BUNDLE_TYPE" == "all" ]
    then
          tail -c +8 "${original_aib_path}" > "${TEMP_TAR_FILE}" && cat "${TEMP_TAR_FILE}" > ${BACKENDAIB}
          # this removes the magic header from the .aib
          # making it usable with the tar command
          rm -f ${TEMP_TAR_FILE}    
    fi

    rm -f "${original_aib_path}"
    if [ "$BUNDLE_TYPE" == "upgradefrontends" ] || [ "$BUNDLE_TYPE" == "all" ]
    then
        outfile=${ORIGINAL_TARBALL:-${TARBALL_PATH}}
        bname=$(basename "${outfile}")
        echo "frontend_aib_dest_file = \"/var/tmp/${bname}\""  > ${FRONTENDAIB_TFVARS}
        echo "frontend_aib_local_file = \"${bname}\"" >> ${FRONTENDAIB_TFVARS}
    fi

    if [ "$BUNDLE_TYPE" == "upgradebackends" ] || [ "$BUNDLE_TYPE" == "all" ]
    then
        outfile_backend=${ORIGINAL_TARBALL:-${BACKENDAIB}}
        backend_name=$(basename "${outfile_backend}")
        echo "backend_aib_dest_file = \"/var/tmp/${backend_name}\"" > ${BACKENDAIB_TFVARS} 
        echo "backend_aib_local_file = \"${backend_name}\"" >> ${BACKENDAIB_TFVARS}
    fi
  else
    echo "✘ ERROR"
    cat /tmp/thelog.log
    exit 1
  fi
  

  if [ -f "${MANIFEST_PATH}" ]; then
    create_manifest_auto_tfvars
  else
    hardcode_manifest_auto_tfvars
  fi

}

exec_linux() {
  download_automate_cli
  new_airgap_bundle_create
}

# We are creating a2ha_manifest.auto.tfvars as they will be used by terraform modules while deployment
create_manifest_auto_tfvars(){
  cat >"${MANIFEST_TFVARS}" <<EOL
  $(echo "pgleaderchk_pkg_ident =$(grep "automate-backend-pgleaderchk" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "postgresql_pkg_ident =$(grep "automate-backend-postgresql" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "proxy_pkg_ident = $(grep "automate-backend-haproxy" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "journalbeat_pkg_ident = $(grep "automate-backend-journalbeat" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "metricbeat_pkg_ident = $(grep "automate-backend-metricbeat" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "kibana_pkg_ident = $(grep "automate-backend-kibana" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "elasticsearch_pkg_ident = $(grep "automate-backend-elasticsearch" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "elasticsidecar_pkg_ident = $(grep "automate-backend-elasticsidecar" ${MANIFEST_PATH})" | sed 's/,*$//g')
  $(echo "curator_pkg_ident = $(grep "automate-backend-curator" ${MANIFEST_PATH})" | sed 's/,*$//g')
EOL
}

hardcode_manifest_auto_tfvars(){
    # We are hardcoding this value as of now for a2ha_manifest.auto.tfvars, because when we will be moving this in automate repo there will be automate manifest file so 
   # from that file we will be creating the a2ha_manifest.auto.tfvars like we are doing in elif section 
    echo "
    pgleaderchk_pkg_ident    = \"chef/automate-backend-pgleaderchk/1.0.22/20201118194223\"
    postgresql_pkg_ident     = \"chef/automate-backend-postgresql/1.0.22/20201118194223\"
    proxy_pkg_ident          = \"chef/automate-backend-haproxy/1.0.22/20201118194223\"
    journalbeat_pkg_ident    = \"chef/automate-backend-journalbeat/1.0.22/20201118194223\"
    metricbeat_pkg_ident     = \"chef/automate-backend-metricbeat/1.0.22/20201118194223\"
    kibana_pkg_ident         = \"chef/automate-backend-kibana/1.0.22/20201118194223\"
    elasticsearch_pkg_ident  = \"chef/automate-backend-elasticsearch/1.0.22/20201118194201\"
    elasticsidecar_pkg_ident = \"chef/automate-backend-elasticsidecar/1.0.22/20201118194223\"
    curator_pkg_ident        = \"chef/automate-backend-curator/1.0.22/20201118193951\"
   " > ${MANIFEST_TFVARS}
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
  touch ${TARBALL_PATH}
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
while getopts ":b:d:t:w:o:h:v:q:" opt; do
  case "${opt}" in
    d)
      export CHEF_AUTOMATE_BIN_PATH=${OPTARG}
      ;;
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
# if [[ -z ${TARBALL_PATH} ]]; then
#   echo "ERROR: required option -o not specified!"
#   usage
# fi
# Set up some path context variables
ABSOLUTE_PATH=$(abs_path "${0}")
export ABSOLUTE_PATH
REPO_PATH=$(dirname "$(dirname "${ABSOLUTE_PATH}")")
export REPO_PATH
do_tasks
clean_up "$@"
