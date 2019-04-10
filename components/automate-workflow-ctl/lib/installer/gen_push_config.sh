#!/bin/bash

# Exit on any error.
set -e

# enable job control: needed for disown below
set -m

# pidof and other useful commands live in sbin, which isn't included on the path
# by default on all versions of Centos/RHEL
PATH=$PATH:/sbin:/usr/sbin

# This script configures push client to run as a service,
# It expects the following conditions to be met:
# * chefdk installed to /opt/chefdk
# * node has been bootstrapped, and a correct configuration
#   exists it /etc/chef/client.rb
mkdir -p /var/log/push-jobs-client

# If we're running in a Docker container, don't bother talking to an Init system.
# Note: This may change in the future if we figure out how to build a Docker
# container that can support init systems.
# Catch failure case so we don't crash the script since set -e is on.
output=`awk -F/ '$2 == "docker"' /proc/self/cgroup | read` && running_in_docker=0 || running_in_docker=1; true

if [ "$running_in_docker" -eq "0" ]; then
  echo "Running in a Docker container, not registering the service."
  showtime="false"
else
  systemd_pid=`pidof systemd||echo ""`
  if [ "$systemd_pid" = "" ]; then
    # Install init script based on current platform family
    family=`/opt/chefdk/embedded/bin/ohai platform_family | grep '"'`
    case $family in
      *debian*)
        cp ./push-jobs-client-ubuntu-upstart /etc/init/push-jobs-client.conf
        initctl reload-configuration
        ;;
      *rhel*)
        cp ./push-jobs-client-rhel-6 /etc/rc.d/init.d/push-jobs-client
        chmod ugo+x /etc/rc.d/init.d/push-jobs-client
        # Register this as a service
        chkconfig --add push-jobs-client
        ;;
    esac
    showtime="true"
  else
    cp ./push-jobs-client-systemd /etc/systemd/system/push-jobs-client.service
    systemctl start push-jobs-client
    systemctl enable push-jobs-client
    showtime="false"
  fi
fi

# Generate push client configuration based on delivery whitelist requirements
# and info from /etc/chef/client.rb.
chef_server=`grep chef_server_url /etc/chef/client.rb | awk '{print $2}'`
node_name=`grep node_name /etc/chef/client.rb | awk '{print $2}'`
client_key=`grep client_key /etc/chef/client.rb | awk '{print $2}'`
trusted_certs_dir=`grep trusted_certs_dir /etc/chef/client.rb | awk '{print $2}'`

(
cat <<EOF

chef_server_url   $chef_server

allow_unencrypted true

# Chef server connect options
node_name         $node_name
client_key        $client_key
trusted_certs_dir $trusted_certs_dir
verify_api_cert   true
ssl_verify_mode   :verify_peer
whitelist({"chef-client"=>"chef-client",
           /^delivery-cmd (.+)$/=>"/var/opt/delivery/workspace/bin/delivery-cmd '\\\\1'"})

# NOTE - if we settle on a daemon provider that captures and timestamps logs,
#        set this to 'false' to avoid duplicated timestamps in the logs.
Mixlib::Log::Formatter.show_time = $showtime
EOF
) > /etc/chef/push-jobs-client.rb

block_until_running() {
  for i in $(seq 1 10); do
    if ! service push-jobs-client status; then
      echo "waiting for push-jobs-client to start; sleeping 1s ($i/10)"
      sleep 1
    else
      exit 0
    fi
  done
  # if we get here, it failed
  echo "push-jobs-client didn't come up after 10s, exiting"
  exit 1
}

if [ "$running_in_docker" -eq "0" ]; then
  # Start push jobs in the background
  nohup /opt/chefdk/bin/pushy-client -l info -c /etc/chef/push-jobs-client.rb &>/dev/null &
else
  # don't forward SIGHUP to the backgrounded process
  service push-jobs-client start & disown -h
  # when this has finished, it doesn't mean the service was started properly,
  # but this will catch failures in service startup
  block_until_running
fi
