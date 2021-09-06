#!/bin/bash -ex
# This script is meant to be run once at VM provision time only

yum install -y nfs-utils
mkdir -p ${mount_point}

echo '172.31.7.151:/mnt/tank/NFS/ ${mount_point} nfs rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2,_netdev 0 0' >> /etc/fstab
mount ${mount_point}
