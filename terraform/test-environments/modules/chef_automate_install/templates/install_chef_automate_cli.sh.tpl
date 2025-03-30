#!/bin/bash
set -e
# NOTE: This is a terraform template. The ${upgrade} and ${channel}
# variables will be replaced with strings at rendering time.  Those
# are not shell variables.
export HAB_NONINTERACTIVE="true"
export HAB_NOCOLORING="true"
export HAB_LICENSE=accept-no-persist
export PATH="/usr/local/bin:$PATH"

automate_deployed() {
    [[ -f /hab/user/deployment-service/config/user.toml ]]
}

automate_needs_redeploy() {
    automate_deployed && ! systemctl list-unit-files chef-automate.service --no-legend | grep -q chef-automate.service
}

upgrade_automate() {
    [[ "${upgrade}" == "true" ]]
}

deploy() {
    mkdir -p /etc/chef-automate
    cp /tmp/chef-automate-config.toml /etc/chef-automate/config.toml
    chmod a+rx /var/opt
    deploy_options="/etc/chef-automate/config.toml"
    deploy_options="$deploy_options --accept-terms-and-mlsa"
    deploy_options="$deploy_options --admin-password ${admin_password}"
    if [[ "${airgapped}" == "true" ]]; then
        deploy_options="$deploy_options --airgap-bundle /tmp/automate.aib"
    fi
    chef-automate deploy $deploy_options
    if [[ "${airgapped}" == "true" ]]; then
        rm -f /tmp/automate.aib
    fi
    configure_retention
}

redeploy() {
    mkdir -p /etc/chef-automate
    cp /tmp/chef-automate-config.toml /etc/chef-automate/config.toml
    chmod a+rx /var/opt
    chef-automate deploy /etc/chef-automate/config.toml --accept-terms-and-mlsa --skip-preflight
    configure_retention
}

install_automate_cmd() {
    # Perform the installation
    pushd "/tmp"
    curl -s https://packages.chef.io/files/${channel}/automate/latest/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    mv chef-automate /usr/local/bin/chef-automate
    popd
}

install_inspec() {
    # install the latest inspec from omnitruck if it doesn't exist
    # this currently only gets run on the hardened fresh install instances
    # so we're always going to have the latest inspec
    if ! [ -x "$(command -v inspec)" ]; then
	curl -s https://omnitruck.chef.io/install.sh | bash -s -- -P inspec -v 4.16.0
    fi
}

wait_for_upgrade() {
    # 60 tries, 10 seconds between tries.  Roughly 10 minutes + the
    # time of the commands
    upgrade_complete="false"
    for try in {1..60}; do
        echo "Checking upgrade status (try $try/60)"
        if chef-automate upgrade status | grep -e "up-to-date" -e "Chef Automate upgraded"; then
            upgrade_complete="true"
            break
        else
            echo "Retrying in 10 seconds"
            sleep 10
        fi
    done

    if [[ "$upgrade_complete" != "true" ]]; then
        echo "Services failed to upgrade in a reasonable amount of time."
        exit 1
    fi
}

hardened_security_inspec_scan() {
    install_inspec
    CHEF_LICENSE="accept-no-persist" inspec exec /tmp/a2-hardened-security || exit_status=$?
    if [[ $exit_status -ne 0 && $exit_status -ne 101 ]]; then
        exit $exit_status
    fi
}

configure_retention() {
  chef-automate dev grpcurl ingest-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
    "recurrence":"FREQ=DAILY;DTSTART=20190820T174501Z;INTERVAL=1",
    "policy_update": {
      "es": [
        {
          "policy_name":"converge-history",
          "older_than_days":"${retention_older_than_days}"
        },
        {
          "policy_name":"actions",
          "older_than_days":"${retention_older_than_days}"
        }
      ]
    }
  }'

  chef-automate dev grpcurl compliance-service -- chef.automate.infra.data_lifecycle.api.Purge.Configure -d '{
    "recurrence":"FREQ=DAILY;DTSTART=20190820T174501Z;INTERVAL=1",
    "policy_update": {
      "es": [
        {
          "policy_name":"compliance-scans",
          "older_than_days":"${retention_older_than_days}"
        },
        {
          "policy_name":"compliance-reports",
          "older_than_days":"${retention_older_than_days}"
        }
      ]
    }
  }'
}

configure_automate_infra_views() {
  if chef-automate dev grpcurl automate-gateway list | grep "chef.automate.api.infra_proxy.InfraProxy" &> /dev/null; then
      chef_server_admin_key="$(</hab/chef-server-admin-key.txt tr '\n' ':' | sed 's/:/\\n/g')"
      server_id="auto-deployed-chef-server"
      org_id="auto-deployed-chef-org"
      if ! chef-automate dev grpcurl automate-gateway -- chef.automate.api.infra_proxy.InfraProxy.GetServer -d "{\"id\": \"$server_id\"}" 2> /dev/null | grep "$server_id" &> /dev/null; then
          chef-automate dev grpcurl automate-gateway -- chef.automate.api.infra_proxy.InfraProxy.CreateServer -d @ <<EOM
          {
            "id": "$server_id",
            "name": "$server_id",
            "fqdn": "localhost",
            "ip_address": "127.0.0.1"
          }
EOM
      fi

      if ! chef-automate dev grpcurl automate-gateway -- chef.automate.api.infra_proxy.InfraProxy.GetOrg -d "{\"id\": \"$org_id\", \"server_id\": \"$server_id\"}" 2> /dev/null | grep "$org_id" &> /dev/null; then
          chef-automate dev grpcurl automate-gateway -- chef.automate.api.infra_proxy.InfraProxy.CreateOrg -d @ <<EOM
          {
            "id": "$org_id",
            "name": "${chef_server_org}",
            "admin_user": "${chef_server_admin_name}",
            "admin_key": "$chef_server_admin_key",
            "server_id": "$server_id"
          }
EOM
      fi
  fi
}

if [[ "${airgapped}" == "false" ]]; then
    if ! command -v unzip &> /dev/null; then
        command -v apt-get &> /dev/null && apt-get install -y unzip
        command -v yum &> /dev/null && yum install -y unzip
    fi
fi

if [[ "${hardened_security}" == "true" ]]; then
    iptables -A INPUT -p tcp -m tcp --dport 80 -m state --state NEW -j ACCEPT
    iptables -A INPUT -p tcp -m tcp --dport 443 -m state --state NEW -j ACCEPT
    iptables-save > /etc/sysconfig/iptables

    hardened_security_inspec_scan
fi

if [[ "${airgapped}" == "false" ]]; then
    if (! automate_deployed) || upgrade_automate || automate_needs_redeploy; then
        echo "installing automate cli"
        install_automate_cmd
    fi
fi

if ! automate_deployed; then
    echo "deploying automate"
    deploy
else
    if automate_needs_redeploy; then
        echo "redeploying automate"
        redeploy
    fi
    if upgrade_automate; then
        echo "inside upgrade_automate"
        if [[ "${airgapped}" == "true" ]]; then
            echo "inside upgrade_automate airgapped true"
            ERROR=$(chef-automate upgrade run --airgap-bundle /tmp/automate.aib 2>&1 >/dev/null) || true
            if echo "$ERROR" | grep 'This is a Major upgrade'; then
              echo "inside upgrade_automate airgapped true major upgrade"
              echo "y
y
y
y
y
y" | chef-automate upgrade run --major --airgap-bundle /tmp/automate.aib
              sleep 45
              #shellcheck disable=SC2154
              wait_for_upgrade
              echo "y" | chef-automate post-major-upgrade migrate --data=ES
            else
              echo "regular normal upgrade airgap"
              sleep 45
              #shellcheck disable=SC2154
              wait_for_upgrade
            fi
            rm -f /tmp/automate.aib
        else
          echo "inside upgrade_automate airgapped false"
          ERROR=$(chef-automate upgrade run 2>&1 >/dev/null) || true
          if echo "$ERROR" | grep 'This is a Major upgrade'; then
            echo "inside upgrade_automate airgapped false major upgrade"
            echo "y
y
y
y
y
y" | chef-automate upgrade run --major
            sleep 45
            #shellcheck disable=SC2154
            wait_for_upgrade
            echo "y" |chef-automate post-major-upgrade migrate --data=ES
          else
            echo "regular normal upgrade airgap false"
            sleep 45

            #shellcheck disable=SC2154
            wait_for_upgrade
          fi
        fi

        cp /tmp/chef-automate-config.toml /etc/chef-automate/config.toml
        chef-automate config set /etc/chef-automate/config.toml
    fi
fi

hab license accept

# Update to whatever the latest version of hab that got installed is
hab pkg binlink core/hab --force

chef-automate dev create-iam-dev-users

if [[ "${create_admin_token}" == "true" ]]; then
    if [[ ! -f /root/admin-token.txt ]]; then
        date +%s | xargs -I % chef-automate iam token create admin-token-% --admin | tr -d '\n' > /root/admin-token.txt
    fi
    cp /root/admin-token.txt $(hab pkg path chef/automate-ui)/dist/
fi

if [[ "${hardened_security}" == "true" ]]; then
    hardened_security_inspec_scan
fi

if [[ "${enable_chef_server}" == "true" ]]; then
  # install an unstable release of chef-dk that includes a berks patch that we
  # need for cookbook uploads.
  #
  # https://github.com/berkshelf/berkshelf/pull/1789
  #
  # NOTE: we can move back to stable when version 3.2.5 or newer of the chef-dk
  # is promoted to the stable channel
  #
  if [[ ! -d "/hab/pkgs/chef/chef-dk/3.2.5/20180806224746" ]]; then
    hab pkg install chef/chef-dk/3.2.5/20180806224746 -c unstable
  fi
  hab pkg binlink chef/chef-dk berks

  cat << EOH > /tmp/.berks.config.json
{
  "chef": {
    "chef_server_url":        "https://localhost/organizations/${chef_server_org}",
    "node_name":              "${chef_server_admin_name}",
    "client_key":             "/hab/chef-server-admin-key.txt"
  },
  "ssl": {
    "verify": false
  }
}
EOH

  cat << EOH > /tmp/.Berksfile
source "https://supermarket.chef.io"
cookbook "audit"
EOH

  if ! chef-server-ctl user-list | grep ${chef_server_admin_name} &> /dev/null; then
    # save the key in /hab so it is on persistent storage that gets remounted even if the EC2 instance is replaced
    chef-server-ctl user-create ${chef_server_admin_name} admin admin admin@admin.com '$3CUR3' -d -f /hab/chef-server-admin-key.txt
  fi
  cp /hab/chef-server-admin-key.txt $(hab pkg path chef/automate-ui)/dist/

  if ! chef-server-ctl org-list | grep ${chef_server_org} &> /dev/null; then
    chef-server-ctl org-create ${chef_server_org} ${chef_server_org} -a ${chef_server_admin_name} -d
  fi

  chef-server-ctl org-user-add ${chef_server_org} ${chef_server_admin_name} -a

  berks install -b /tmp/.Berksfile -c /tmp/.berks.config.json
  berks upload -b /tmp/.Berksfile -c /tmp/.berks.config.json

  # add record to automate infra views to pre-populate deployed chef server.
  configure_automate_infra_views

  if [[ "${enable_workflow}" == "true" ]]; then
    if ! chef-server-ctl user-list | grep delivery &> /dev/null; then
      chef-server-ctl user-create delivery delivery delivery delivery@delivery.com '$3CUR3' -d -f /etc/chef-automate/chef-server-delivery-key.txt
    fi

    if ! chef-server-ctl org-list | grep delivery &> /dev/null; then
      chef-server-ctl org-create delivery delivery -a delivery -d
    fi
  fi
fi

if [[ "${enable_workflow}" == "true" ]]; then
    if ! workflow-ctl list-enterprises | grep "${workflow_enterprise}"; then
        echo "y" | ssh-keygen -t rsa -b 4096 -N '' -f /root/builder_key
        workflow-ctl create-enterprise "${workflow_enterprise}" --ssh-pub-key-file=/root/builder_key.pub
    fi
fi

# Create the chef-ci user (if missing)
if ! getent passwd chef-ci &> /dev/null; then
  sudo useradd chef-ci --create-home
fi

# Add chef-ci's public key to authorized list
sudo mkdir -p /home/chef-ci/.ssh
sudo chown 0700 /home/chef-ci/.ssh
cat << EOH > /tmp/.ssh-chef-ci
ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQC8cKl0sNhpxdOVm2T/3wfwmSExaaDUCNSKJ15D146Y2tQygRdxGY5eHkOrFET8ssnetBFrSB9B+uQYPD9+KpLkupXtL2Sx4EtyuVnUUyoEXgAC7Sr6bxwo+FqfBAkrW1vNakss/WknmaXIDYsHhI16wYTr7nIE41oGPIbcdRDAXp4u56m3tQ2kfiTkg104D2TL50z2YT6I7B1h8CUpz9aAOtd+BYGueX5rdmOATIrPydcLdvWmqrO3GXZKV3zCHG2S/Se+ULC+EhbBMZkICYo3Jre7fedkCIIGhla71h9wg7q6b3eBWowfRWCCKskJ+rkO72zSZsL9EhY/9bcg7leP8hzwmWApeddVVlumqjkPpMkGfU26TKi52gevHW6fsyxCqDR9qhjBOGxSgiqNBQuOEg/9PVLlWBcsrgNhNxsysQEZTi0jv9FdONY3c1zQ+AXHH9HtxjBnx1xD59uzEYG1hUF1MsRwpWswH3Thnd/zbSxKKOdGRqoqy52Icaf7Z96D9XKAOpDQj4pTW0fS3uJP8AL16CNSkHUZSn0vxZCS9lJS+dDxwkDk1NInQqmpJ1NPwoTPlhMEsggqPuzyh+9R38NTE6cIAddq4gqJvbRLvc4jtARoz1D123QuLPTN1Ie41xhHvSI5I2gROz20rfKp57DOuLov5nzlvbb5mH/Z6Q== chef-ci-2021-07-01
EOH
sudo mv /tmp/.ssh-chef-ci /home/chef-ci/.ssh/authorized_keys
sudo chown 0600 /home/chef-ci/.ssh/authorized_keys
sudo chown -R "chef-ci:" /home/chef-ci

# Add chef-ci to sudoers
cat << EOH > /tmp/chef-ci-sudoer
# The chef-ci user is used by InSpec to validate the system
chef-ci ALL=(ALL) NOPASSWD:ALL
EOH
sudo mv /tmp/chef-ci-sudoer /etc/sudoers.d/10-chef-ci
sudo chmod 0440 /etc/sudoers.d/10-chef-ci

# Add chef-ci to sshd_config in hardened image
if [[ "${hardened_security}" == "true" ]]; then
    sudo sed  -i -r "s/^AllowUsers (.*)/AllowUsers \\1 chef-ci/" /etc/ssh/sshd_config
    sudo chmod 0644  /etc/ssh/sshd_config
    sudo systemctl restart sshd.service
fi