#!/bin/bash -x

if  which yum &> /dev/null; then
  echo "yum package manager found; install redhat style packages"
  yum install -y nfs-utils bind-utils
elif which apt &> /dev/null; then
  echo "apt package manager found; install debian style packages"
  until [[ -f /var/lib/cloud/instance/boot-finished ]]; do
    echo "sleeping until cloudinit finishes"
    sleep 1
  done
  apt update
  apt install -y nfs-common dnsutils
else
  echo "No supported package manager found"
  echo "Supported OS's: Redhat, OEL, Centos, Ubuntu"
fi
# end conditional
NFS_SRC="${efs_mount_dns}"
NFS_OPTIONS="nfsvers=4.1,rsize=1048576,wsize=1048576,hard,timeo=600,retrans=2"

mkdir -p ${mount_path}
echo "$NFS_SRC:/ ${mount_path} nfs4 $NFS_OPTIONS 0 0" >> /etc/fstab

# wait until DNS lookups succeed, they don't happen right away
failure() {
  echo "$1"
  exit 1
}
wait_for_efs_dns() {
  max=30
  n=0
  until [ $n -ge $max ]; do
    grep '${mount_path}' /proc/mounts && break
    host $NFS_SRC && mount ${mount_path} && break
    n=$((n+1))
    echo "Waiting EFS mount $NFS_SRC to appear in DNS"
    sleep 2
  done
  if [[ $n -ge $max ]]; then
    failure "Failed waiting for EFS mount $NFS_SRC to appear in DNS"
  fi
}
wait_for_efs_dns

chmod 777 ${mount_path}
