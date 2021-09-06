#!/bin/bash -ex
# This script is meant to be run once at VM provision time only

yum install -y lvm2 xfsprogs

# Create LVM LV /dev/chef-vg/chef-lv
pvcreate "${disk_dev}"
vgcreate chef-vg "${disk_dev}"
lvcreate -n chef-lv -l "${lvm_volume_allocate_pct}%VG" chef-vg

# create xfs
mkfs.xfs /dev/chef-vg/chef-lv

# mount
echo '/dev/chef-vg/chef-lv /hab xfs defaults 0 0' >> /etc/fstab
mkdir -p /hab
mount /hab
