#!/bin/bash
#shellcheck disable=SC2034
test_name="product_mitm"
test_deploy_inspec_profiles=(a2-deploy-integration)
# The inspec tests don't pass if the diagnostics are run
test_skip_diagnostics=true

mitmport="19111"
do_setup() {

    do_setup_default

    local previous_umask
    previous_umask=$(umask)
    umask 022

    # We do not need to manually install mitm proxy from pip because its part
    # of our test container

    adduser mitmproxy
    mkdir /etc/mitmproxy
    chown mitmproxy:root /etc/mitmproxy
    cat > /etc/systemd/system/mitmproxy.service <<EOF
[Unit]
Description=Mitmproxy
After=network.target

[Service]
Type=idle
LimitNOFILE=32768
User=mitmproxy
ExecStart=/usr/local/bin/mitmdump -p "${mitmport}" --mode transparent -k --confdir=/etc/mitmproxy

[Install]
WantedBy=multi-user.target
EOF

    umask "$previous_umask"

    systemctl start mitmproxy
}

do_prepare_deploy() {
    sysctl -w net.ipv4.ip_forward=1
    sysctl -w net.ipv6.conf.all.forwarding=1

    iptables -t nat -A OUTPUT -p tcp -m owner ! --uid-owner mitmproxy \
        --dport 443 -j REDIRECT --to-port "${mitmport}"
    iptables -t nat -A OUTPUT -p tcp -m owner ! --uid-owner mitmproxy \
        --dport 80 -j REDIRECT --to-port "${mitmport}"

    mkdir /etc/systemd/system/chef-automate.service.d

    cat > /etc/systemd/system/chef-automate.service.d/custom.conf <<EOF
[Service]
Environment=SSL_CERT_FILE=/etc/mitmproxy/mitmproxy-ca-cert.pem
EOF

    export SSL_CERT_FILE=/etc/mitmproxy/mitmproxy-ca-cert.pem

    do_prepare_deploy_default
}
