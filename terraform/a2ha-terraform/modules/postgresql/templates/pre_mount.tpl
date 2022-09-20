#!/bin/bash -x

sudo mkdir -p ${nfs_mount_path}/postgresql/pg_dump/

sudo mkdir -p ${nfs_mount_path}/postgresql/archive/

sudo chown -R hab:hab ${nfs_mount_path}/
