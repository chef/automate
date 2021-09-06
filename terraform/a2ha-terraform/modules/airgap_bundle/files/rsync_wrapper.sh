#!/usr/bin/env bash

set -euo pipefail

BANNER="
This script is responsible for using rsync to transfer files

The following flags are available:

 [REQUIRED]
 -i IP            IP/hostname of the destination system
 -k PATH          Path to the ssh key pair file
 -u USER          SSH username on the destination system
 -p PATH          The path to airgap_bundle module (\${path.module})
 -l LIST          Comma delimited list of SRC1,DST1,SRC2,DST2.. file pairs
                  The SOURCE path is relative to terraform/transfer_files/ directory

 [OPTIONS]
 -h             Print this help message

 ex. $0 -i 192.158.24.2 -k ~/.ssh/you.pem -u admin -l backend.aib,/tmp/backend.aib -p $(pwd)/terraform/modules/airgap_bundle
"
export BANNER

usage() {
  echo "$BANNER"
  exit 1
}

if [[ $# -ne 10 ]]; then
  usage
fi

export SSH_KEY_FILE=
export SSH_IP=
export SSH_USER=
export SSH_FILES=()
export MODULE_DIR=

while getopts ":i:k:u:p:l:h" opt; do
  case "${opt}" in
    i)
      export SSH_IP=${OPTARG}
      ;;
    k)
      export SSH_KEY_FILE=${OPTARG}
      ;;
    u)
      export SSH_USER=${OPTARG}
      ;;
    p)
      export MODULE_DIR=${OPTARG}
      ;;
    l)
      set -f # disable glob
      IFS=,
      # shellcheck disable=SC2206
      SSH_FILES=($OPTARG) # use the split+glob operator
      set +f # enable
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

failure() {
  echo "$1" >> /tmp/FAIL.log
  exit 1
}

rsync_with_retry() {
  max=40
  n=0
  until [ $n -ge $max ]; do
    rsync -aW --partial --inplace -e "ssh -o StrictHostKeyChecking=no\
      -o UserKnownHostsFile=/dev/null -i ${SSH_KEY_FILE}" "${1}" \
      "${SSH_USER}@${SSH_IP}:${2}" && break
    n=$((n+1))
    echo "Will re-attempt rsync of ${2} to ${SSH_IP} on next loop iteration.."
    sleep 5
  done
  if [[ $n -ge $max ]]; then
    failure "Rsync of ${1} -> ${2} failed to complete within ${max} iterations!"
  fi
}

for (( i=0; i<${#SSH_FILES[@]} ; i+=2 )) ; do
  # remove any trailing slash
  dir=${MODULE_DIR%/}
  # run all rsyncs in parallel (backgrounded)
  rsync_with_retry "${dir}/../../transfer_files/${SSH_FILES[i]}" "${SSH_FILES[i+1]}" &
done
