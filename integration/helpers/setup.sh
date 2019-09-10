#!/usr/bin/env bash

install_chefdk() {
    local channel="${1:-stable}"
    local version="${2:-latest}"
    curl https://omnitruck.chef.io/install.sh | bash -s -- -c "$channel" -P chefdk -v "$version"
    chef --version
}

start_requestbin() {
    export GOBIN="/go/bin"
    go install integration/helpers/requestbin/requestbin.go
    cat > /etc/systemd/system/requestbin.service <<EOF
[Unit]
Description=Requestbin

[Service]
ExecStart=/go/bin/requestbin

[Install]
WantedBy=default.target
EOF
    systemctl start requestbin
}
start_loadbalancer() {
    export GOBIN="/go/bin"
    go install ./integration/helpers/loadbalancer/
    #shellcheck disable=SC2154
    cat > /etc/systemd/system/loadbalancer.service <<EOF
[Unit]
Description=Load Balancer

[Service]
ExecStart=/go/bin/loadbalancer $(pwd)/$test_config_path $@

[Install]
WantedBy=default.target
EOF
    systemctl start loadbalancer.service
}

start_external_elasticsearch() {
    curl -o elasticsearch.rpm https://artifacts.elastic.co/downloads/elasticsearch/elasticsearch-6.2.2.rpm
    rpm --install elasticsearch.rpm

    cat > /etc/elasticsearch/elasticsearch.yml <<EOF
cluster.name: "external-network"
network.host: 127.0.0.1
http.port: 59200
transport.tcp.port: "59300-59400"
path.repo: "/var/opt/chef-automate/backups"

discovery.zen.minimum_master_nodes: 1
EOF

    adduser hab

    mkdir -p /var/opt/chef-automate/backups/automate-elasticsearch-data
    chmod -R 0777 /var/opt/chef-automate/backups

    mkdir -p /var/run/elasticsearch
    chown elasticsearch:elasticsearch /var/run/elasticsearch
    chown -R elasticsearch:elasticsearch /usr/share/elasticsearch/

    systemctl start elasticsearch.service
}
