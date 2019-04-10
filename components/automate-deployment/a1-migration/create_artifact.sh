#!/bin/bash

set -xe

# Helper script to run on the a1-migration instance to create an artifact safely

# You need a real key one way or the other. We publish packages to the
# `devchef` origin. The devchef-20180312184422 key is available in lastpass.
# If you are only working locally, though, you may simply create a local one
# with `hab origin key generate devchef`
ARTIFACT_TYPE=${1:-"full"}

# install_from_results_or_depot accepts two parameters, the package to install and
# the channel, this function will verify first if you have any local package inside
# the /a1-migration/results folder and install it from there if it exists, otherwise
# we continue installing it from the Depot
function install_from_results_or_depot() {
  local channel=${2:-dev}

  if [ -d /a1-migration/results ]; then
    results_artifact=$(find /a1-migration/results -name "*$1*" -exec ls -1t \{\} \+ | head -1)
    if [ ! -z "$results_artifact" ]; then
      echo "Installing package '$1' from local source. ('/a1-migration/results') "
      hab pkg install -b "$results_artifact"
      return 0
    fi
  fi

  hab pkg install -b "chef/$1" --channel "$channel"
}

if find /a1-migration/hab_keys/ -name devchef*sig.key | grep -c devchef
then
  mkdir -p /hab/cache/keys
  cp /a1-migration/hab_keys/devchef*.sig.key /hab/cache/keys
else
  # Make this error message less ugly:
  set +xe
  echo "ERROR: devchef origin key not found"
  echo "--------------------------------------------------------------------------------"
  echo "You must put a hab key for the devchef origin in a1-migration"
  echo "The real key can be found in last pass; search for 'devchef-20180312184422'"
  echo "If you're only working locally, 'hab origin key generate devchef' will do the trick"
  echo "Then copy the key from ~/.hab/cache/keys to a1-migration/hab_keys/"
  echo "and run this again."
  exit 1
fi

if find /a1-migration/hab_keys/ -name devchef*pub | grep -c devchef
then
  cp /a1-migration/hab_keys/devchef*.pub /hab/cache/keys
else
  # Make this error message less ugly:
  set +xe
  echo "ERROR: devchef origin public key not found"
  echo "--------------------------------------------------------------------------------"
  echo "You must put a hab public key for the devchef origin in a1-migration"
  echo "You can download the key to ~/.hab/cache/keys with the command:"
  echo "\`\`\`"
  echo "hab origin key download devchef"
  echo "\`\`\`"
  echo "then copy them to a1-migration/hab_keys/ and run this command again"
  exit 1
fi

# Create a PG Dump
if [[ "$ARTIFACT_TYPE" = "minimal" ]]; then
  install_from_results_or_depot automate-postgresql dev
  install_from_results_or_depot automate-cli dev
  chef-automate init-config
  chef-automate dev a1-dump-pg -c config.toml --enable-chef-server --enable-workflow
fi

# Make sure state isn't changing when we create the backup
/opt/delivery/bin/delivery-ctl stop || true
/opt/opscode/bin/chef-server-ctl stop || true

# Build the package from the root directory so that the entire container filesystem
# will be mounted into /src in the hab chroot.
if [[ "$ARTIFACT_TYPE" = "minimal" ]]; then
    HAB_PLAN_DIR="a1-migration-data-minimal"
else
    HAB_PLAN_DIR="a1-migration-data-full"
fi

mkdir -p /habitat
cp -a /a1-migration/$HAB_PLAN_DIR/* /habitat
pushd /
  hab pkg build .
popd

# Move the artifacts from the machines /results directory to the /a1-migration
# volume folder so that the build results are accessible locally.
mkdir -p /a1-migration/results
mv /results/*.hart /a1-migration/results/

/opt/delivery/bin/delivery-ctl start || true
/opt/opscode/bin/chef-server-ctl start || true
