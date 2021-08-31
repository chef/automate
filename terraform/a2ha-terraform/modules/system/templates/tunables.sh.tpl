#!/bin/bash

# Note: many of these come from directly from preflight-check
#  however, ip_local_port_range is specifically to ensure ephemeral ports
#   never conflict with assigned ports by Automate:
#   https://github.com/chef/automate/blob/master/components/automate-deployment/PORT_ALLOCATION.md

umask 0022

cat > /etc/sysctl.d/00-chef.conf <<EOF
vm.swappiness=10
vm.max_map_count=262144
vm.dirty_ratio=20
vm.dirty_background_ratio=30
vm.dirty_expire_centisecs=30000
net.ipv4.ip_local_port_range=10300 65024
net.ipv4.tcp_max_syn_backlog=60000
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
