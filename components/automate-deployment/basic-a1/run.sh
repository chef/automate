#!/bin/bash -xe

# run.sh is the entry point script for an A1 migration container. It applies
# A1/Chef server version and configuration changes and configures the the
# machine to run as a hab builder.

export PATH=/opt/opscode/bin:$PATH
export PATH=/opt/delivery/bin:$PATH

# Set sysctl values needed to pass A2 preflight checks
sysctl -w vm.max_map_count=262144
sysctl -w vm.dirty_expire_centisecs=20000

# Run setup to make sure our package versions and configuration are up to date.
chef-apply /basic-a1/setup.rb

# TODO: this is disabled because it tries to import your secret origin key, but
# that doesn't work because we disabled mounting ~/.hab in the container
# because that was breaking other stuff
# ----
# Create a new studio so that hab downloads and installs all the required deps
# for a studio.
#hab studio new
