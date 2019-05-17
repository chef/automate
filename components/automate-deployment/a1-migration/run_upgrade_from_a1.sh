#!/bin/bash

set -xe

HARTIFACT_DIR=${HARTIFACT_DIR:-/a1-migration/results}

# Helper script to run on the a1-migration instance to launch an A1 -> A2 upgrade

# install_from_results_or_depot accepts three parameters, the package
# to install, the channel, and the hartifact directory. This function
# will verify first if you have any local package inside the hartifact
# directory and install it from there if it exists, otherwise we
# continue installing it from the Depot.
function install_from_results_or_depot() {
  local package=$1
  local channel=${2:-dev}
  local hart_dir=${3:-/a1-migration/results}

  if [ -d "$hart_dir" ]; then
    results_artifact=$(find "$hart_dir" -name "*$package*" -exec ls -1t \{\} \+ | head -1)
    if [ ! -z "$results_artifact" ]; then
      echo "Installing package '$package' from local source ('$results_artifact')."
      HAB_LICENSE=accept-no-persist hab pkg install -b /bin "$results_artifact"
      return 0
    fi
  fi

  HAB_LICENSE=accept-no-persist hab pkg install -b /bin "chef/$package" --channel "$channel"
}

install_from_results_or_depot automate-cli dev "$HARTIFACT_DIR"
chef-automate upgrade-from-v1 \
	--channel dev \
	--upgrade-strategy none \
	--override-origin "${HAB_ORIGIN}" \
	--hartifacts "${HARTIFACT_DIR}" \
	--enable-chef-server \
	--enable-workflow \
	--skip-backup-check \
	--yes
