#!/bin/bash

set -e

echo "Listing stuff"
ls -lah /services

echo "Doing sysctl stuff"
cat > /etc/sysctl.d/00-chef.conf <<EOF
vm.swappiness=10
vm.max_map_count=262144
vm.dirty_ratio=20
vm.dirty_background_ratio=30
vm.dirty_expire_centisecs=30000
net.core.somaxconn=1024
EOF

sysctl -p /etc/sysctl.d/00-chef.conf

if [[ -d "/proc/net/ipv4" ]]; then
    cat > /etc/sysctl.d/01-ipv4.conf <<EOF
net.ipv4.ip_local_port_range=1024 65024
net.ipv4.tcp_max_syn_backlog=60000
net.ipv4.tcp_tw_reuse=1
EOF
    sysctl -p /etc/sysctl.d/01-ipv4.conf
fi

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
Environment=HAB_LICENSE=accept-no-persist
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

# Needs to be at least 0.75 to get nested config support
# for automate-ha-backend
echo "Installing latest hab"
HAB_LICENSE="accept-no-persist" hab pkg install core/hab
HAB_LICENSE="accept-no-persist" hab pkg binlink core/hab --force

echo "Starting Habitat"
systemctl daemon-reload
systemctl enable hab-sup.service
systemctl start hab-sup.service

channel="dev"
# To pin the version, set version to your desired version, including a
# leading /. For example:
#
# version="/0.1.150"
version=""

PG_PKG_NAME="automate-ha-postgresql"
postgresql_pkg_ident="chef/$PG_PKG_NAME"
PGLEADERCHK_PKG_NAME="automate-ha-pgleaderchk"
pgleaderchk_pkg_ident="chef/$PGLEADERCHK_PKG_NAME"
proxy_pkg_ident="chef/automate-ha-haproxy"
ELASTICSEARCH_PKG_NAME="automate-ha-elasticsearch"
elasticsearch_pkg_ident="chef/automate-ha-elasticsearch"
ELASTICSIDECAR_PKG_NAME="automate-ha-elasticsidecar"
elasticsidecar_pkg_ident="chef/$ELASTICSIDECAR_PKG_NAME"

echo "Installing HA Backend Habitat packages from $channel"
HAB_LICENSE="accept-no-persist" hab pkg install --channel ${channel} "${elasticsearch_pkg_ident}${version}"
HAB_LICENSE="accept-no-persist" hab pkg install --channel ${channel} "${proxy_pkg_ident}${version}"
HAB_LICENSE="accept-no-persist" hab pkg install --channel ${channel} "${pgleaderchk_pkg_ident}${version}"
HAB_LICENSE="accept-no-persist" hab pkg install --channel ${channel} "${postgresql_pkg_ident}${version}"
HAB_LICENSE="accept-no-persist" hab pkg install --channel ${channel} "${elasticsidecar_pkg_ident}${version}"

echo "Copying certs into place"
hostname=$(hostname)

# copy the certs to the correct names
mv "/certificates/odfe-$hostname.pem" /certificates/odfe-node.pem

echo "Configuring HA Backend Services"
mkdir -p "/hab/user/${ELASTICSEARCH_PKG_NAME}/config/"
cat > "/hab/user/${ELASTICSEARCH_PKG_NAME}/config/user.toml" <<EOF
[runtime]
es_java_opts = "-Xms1024m -Xmx1024m"

[es_yaml.network]
host = "0.0.0.0"

[es_yaml.transport]
host = "0.0.0.0"

[es_yaml.bootstrap]
memory_lock = false

[es_yaml.path]
repo = "/services/ha_backend_backups"

[es_yaml.discovery.zen.ping.unicast]
hosts = ["$(head -n 1 /services/ha_backend_peers)"]
[es_yaml.cluster.routing.allocation.disk.watermark]
low = "95%"
high = "98%"
flood_stage = "99%"

[es_yaml.opendistro_security]
enable_snapshot_restore_privilege = true

[opendistro_ssl]

# root pem cert that signed the two cert/key pairs below
rootCA = """$(cat /certificates/MyRootCA.pem)"""

# Certificate used for admin actions against https://9200
admin_cert   = """$(cat /certificates/odfe-admin.pem)"""
admin_key    = """$(cat /certificates/odfe-admin.key)"""

# Certificate used for intracluster ssl on port 9300
ssl_cert    = """$(cat /certificates/odfe-node.pem)"""
ssl_key     = """$(cat /certificates/odfe-node.key)"""
EOF

mkdir -p "/hab/user/${ELASTICSIDECAR_PKG_NAME}/config/"
cat > "/hab/user/${ELASTICSIDECAR_PKG_NAME}/config/user.toml" <<EOF
elasticsearch_ip = "127.0.0.1"
EOF

mkdir -p "/hab/user/${PG_PKG_NAME}/config/"
cat > "/hab/user/${PG_PKG_NAME}/config/user.toml" <<EOF
[superuser]
password = 'thisisapassword%u'
[ssl]
enable = true
ssl_cert    = """$(cat /certificates/postgresql.pem)"""
ssl_key     = """$(cat /certificates/postgresql.key)"""
EOF

echo "Starting HA Backend Habitat services"
HAB_LICENSE="accept-no-persist" hab svc load ${postgresql_pkg_ident} --topology leader --channel ${channel}
HAB_LICENSE="accept-no-persist" hab svc load ${pgleaderchk_pkg_ident} --bind database:"$PG_PKG_NAME".default --binding-mode=relaxed --channel ${channel}
HAB_LICENSE="accept-no-persist" hab svc load ${proxy_pkg_ident} --bind database:"$PG_PKG_NAME".default --bind pgleaderchk:"$PGLEADERCHK_PKG_NAME".default --binding-mode=relaxed --channel ${channel}
HAB_LICENSE="accept-no-persist" hab svc load ${elasticsearch_pkg_ident} --channel ${channel}
HAB_LICENSE="accept-no-persist" hab svc load ${elasticsidecar_pkg_ident} --bind elasticsearch:"$ELASTICSEARCH_PKG_NAME".default --binding-mode=relaxed --channel ${channel}
