#!/bin/bash

set -e

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

echo "Setting up Habitat"
groupadd hab
useradd -g hab hab

cat > /etc/systemd/system/hab-sup.service <<EOF
[Unit]
Description=Habitat-Supervisor
After=network-online.target

[Service]
Type=simple
ExecStartPre=-/bin/rm -f /hab/sup/default/LOCK
ExecStart=/bin/hab sup run --peer-watch-file /services/ha_backend_peers
Restart=on-failure
RestartSec=10
LimitNOFILE=262144
KillMode=process

[Install]
WantedBy=multi-user.target
EOF
chmod 664 /etc/systemd/system/hab-sup.service

echo "Starting Habitat"
systemctl daemon-reload
systemctl enable hab-sup.service
systemctl start hab-sup.service

echo "Installing HA Backend Habitat packages"
channel="unstable"
PG_PKG_NAME="automate-backend-postgresql"
postgresql_pkg_ident="chef/$PG_PKG_NAME"
PGLEADERCHK_PKG_NAME="automate-backend-pgleaderchk"
pgleaderchk_pkg_ident="chef/$PGLEADERCHK_PKG_NAME"
proxy_pkg_ident="chef/automate-backend-haproxy"
ELASTICSEARCH_PKG_NAME="automate-backend-elasticsearch"
elasticsearch_pkg_ident="chef/automate-backend-elasticsearch"
NGINX_PKG_NAME="automate-backend-nginx"
nginx_pkg_ident="chef/$NGINX_PKG_NAME"

hab pkg install --channel ${channel} "${elasticsearch_pkg_ident}"
hab pkg install --channel ${channel} "${proxy_pkg_ident}"
hab pkg install --channel ${channel} "${pgleaderchk_pkg_ident}"
hab pkg install --channel ${channel} "${postgresql_pkg_ident}"
hab pkg install --channel ${channel} "${nginx_pkg_ident}"

echo "Configuring HA Backend Services"
mkdir -p "/hab/user/${ELASTICSEARCH_PKG_NAME}/config/"
cat > "/hab/user/${ELASTICSEARCH_PKG_NAME}/config/user.toml" <<EOF
[runtime]
es_java_opts = "-Xms1024m -Xmx1024m"

[es_yaml.network]
host = "_local_"

[es_yaml.transport]
host = "_site_"

[es_yaml.bootstrap]
memory_lock = false

[es_yaml.discovery.zen.ping.unicast]
hosts = ["$(cat /services/ha_backend_peers | head -n 1)"]
[es_yaml.cluster.routing.allocation.disk.watermark]
low = "95%"
high = "98%"
flood_stage = "99%"
EOF

mkdir -p "/hab/user/${NGINX_PKG_NAME}/config/"
cat > "/hab/user/${NGINX_PKG_NAME}/config/user.toml" <<EOF
listen_ip = "$1"
system_fqdn = "elasticsearch"
client_max_body_size = 250
EOF

cp /services/ha_backend_private/htpasswd /hab/user/"$NGINX_PKG_NAME"/config/.elasticsearch_htpasswd
cp /services/ha_backend_private/nginx-selfsigned* /hab/user/"$NGINX_PKG_NAME"/config/

echo "Starting HA Backend Habitat services"
hab svc load ${postgresql_pkg_ident} --topology leader --channel ${channel}
hab svc load ${pgleaderchk_pkg_ident} --topology leader --bind database:"$PG_PKG_NAME".default --binding-mode=relaxed --channel ${channel}
hab svc load ${proxy_pkg_ident} --topology leader --bind database:"$PG_PKG_NAME".default --bind pgleaderchk:"$PGLEADERCHK_PKG_NAME".default --binding-mode=relaxed --channel ${channel}
hab svc load ${elasticsearch_pkg_ident} --topology leader --bind elasticsearch:"$ELASTICSEARCH_PKG_NAME".default --binding-mode=relaxed --channel ${channel} 
hab svc load ${nginx_pkg_ident} --channel ${channel}
