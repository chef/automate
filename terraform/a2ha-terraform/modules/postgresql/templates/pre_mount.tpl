#!/bin/bash -x

sudo mkdir -p ${nfs_mount_path}/postgresql/pg_dump/

sudo mkdir -p ${nfs_mount_path}/postgresql/archive/

if [ ! -e "/hab/user/automate-ha-postgresql/config/user.toml" ]; then
  sudo chown hab:hab -RL ${nfs_mount_path}/
fi
