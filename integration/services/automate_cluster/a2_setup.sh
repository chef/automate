#!/bin/bash

set -e

if [ ! -e /services/ha_backend.toml ]; then
    echo "automate cluster depends on ha backend cluster"
    exit 1
fi

echo "Doing sysctl stuff"
cat > /etc/sysctl.d/00-chef.conf <<EOF
vm.swappiness=10
vm.max_map_count=262144
vm.dirty_ratio=20
vm.dirty_background_ratio=30
vm.dirty_expire_centisecs=30000
net.ipv4.ip_local_port_range=1024 65024 net.ipv4.tcp_max_syn_backlog=60000
net.ipv4.tcp_tw_reuse=1
net.core.somaxconn=1024
EOF

sysctl -p /etc/sysctl.d/00-chef.conf

cat > /etc/security/limits.d/20-nproc.conf<<EOF
*   soft  nproc     65536
*   hard  nproc     65536
*   soft  nofile    1048576
*   hard  nofile    1048576
EOF

if [ -z "$HAB_ORIGIN" ]; then
    HAB_ORIGIN="chef"
fi

HARTIFACTS_PATH="$PWD/results"
A2_CONFIG_PATH="/services/automate_cluster_private/config.toml"

if [ ! -e "$A2_CONFIG_PATH" ]; then
    echo "Rendering config"
    /services/automate_cluster_private/chef-automate init-config \
        --channel dev \
        --file "$A2_CONFIG_PATH" \
        --upgrade-strategy "at-once"

    cat /services/ha_backend.toml >> $A2_CONFIG_PATH
fi


echo "Starting automate container"
/services/automate_cluster_private/chef-automate deploy $A2_CONFIG_PATH \
        --hartifacts "$HARTIFACTS_PATH" \
        --override-origin "$HAB_ORIGIN" \
        --admin-password chefautomate \
        --accept-terms-and-mlsa
