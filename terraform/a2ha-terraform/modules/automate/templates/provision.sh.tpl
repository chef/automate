#!/usr/bin/env bash

set -euo pipefail

umask 0022

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist
export HAB_SUP_GATEWAY_AUTH_TOKEN=${hab_sup_http_gateway_auth_token}

failure() {
  echo "$1"
  exit 1
}

# Creating mount path for elasticsearch backup 
sudo mkdir -p ${nfs_mount_path}
sudo chown hab:hab -RL ${nfs_mount_path}/

# When we truncate the file we need to preserve the file permisssions
truncate_with_timestamp() {
  local TMPFILE=$(mktemp)
  local FILE=$1
  touch -r $FILE $TMPFILE;
  truncate -cs 0 $FILE
  touch -r $TMPFILE $FILE
  rm $TMPFILE
}
export -f truncate_with_timestamp

save_space() {
  # this will truncate all but the most recent frontend .aib files
  find /var/tmp/ -name frontend\*aib -printf "%T+\t%p\n" | sort -r | awk '{print $2}' | tail -n +2 | xargs -n1 -I{} bash -c 'truncate_with_timestamp "$@"' _ {}
}

wait_for_install() {
  cmd=$1
  max=20
  n=0
  until [ $n -ge $max ]; do
    if $1; then
      break
    fi
    n=$((n+1))
    echo "Waiting for $1 cli to be installed.."
    sleep 30
    if [ $cmd == "automate-backend-ctl" ]
      then
        echo "bin-linking automate-backend-ctl"
        pkg_count=$(ls -Art /hab/cache/artifacts/chef-automate-ha-ctl* | tail -n 1 | wc -l )
        pkg_name=$(ls -Art /hab/cache/artifacts/chef-automate-ha-ctl* | tail -n 1)
        echo "Installing the package $pkg_name"
        echo "pkg_count:$pkg_count"
        echo "pkg_name:$pkg_name"
        if [ $pkg_count -eq 1 ]
          then
            hab pkg install $pkg_name -bf 
        fi
    fi    
  done
  if [[ $n -ge $max ]]; then
    failure "Timed out waiting for $1 to be installed within $max iterations!"
  fi
}

wait_for_frontend_aib() {
  max=20
  n=0
  until [ $n -ge $max ]; do
    if chef-automate airgap bundle info ${frontend_aib_file} 2>/dev/null; then
      break
    fi
    n=$((n+1))
    echo "Waiting for chef-automate airgap bundle info ${frontend_aib_file} to complete.."
    sleep 30
  done
  if [[ $n -ge $max ]]; then
    failure "Timed out waiting for ${frontend_aib_file} to be transferred within $max iterations!"
  fi
}

wait_for_backend_aib() {
  max=20
  n=0
  until [ $n -ge $max ]; do
    ls ${backend_aib_file}.DONE >/dev/null 2>&1  && break
    n=$((n+1))
    echo "Waiting for habitat package installations from ${backend_aib_file} to complete"
    sleep 30
  done
  if [[ $n -ge $max ]]; then
    echo "Failed waiting for ${backend_aib_file} to be transferred within $max iterations!"
    exit 1
  fi
}

wait_for_backend_ctl() {
  # TODO: instead of looping here, we should increase the already existing retry loops in the automate-ha-ctl gem
  max=20
  n=0
  until [ $n -ge $max ]; do
    if hab pkg exec chef/automate-ha-ctl automate-backend-ctl connect --conf-out ${tmp_path}/automate_conf.toml --toml=${tmp_path}/connector.toml --erb=${tmp_path}/config.toml.erb 2>>${tmp_path}/automate-ctl.log; then
      break
    fi
    n=$((n+1))
    echo "Waiting for automate-ha-ctl to connect to backends.."
    sleep 30
  done
  if [[ $n -ge $max ]]; then
    failure "Timed out waiting automate-ha-ctl to within $max iterations!"
  fi
}

# Copied how it's handled in Automate's deployment.sh script here:
# https://github.com/chef/automate/blob/c8dd203818e29803685cf2f2be52bb66fd136037/integration/helpers/deployment.sh#L58
wait_for_upgrade() {
  local upgrade_complete
  upgrade_complete="false"
  echo "Waiting for services to finish upgrading"
  for try in {1..60}; do
    echo "Checking upgrade status (try $try/90)"

    local upgrade_status_output
    local errcode
    errcode="0"
    upgrade_status_output="$(chef-automate upgrade status -d 2>&1)" || errcode="$?"
    echo "$upgrade_status_output"
    echo "status exit code=$errcode"

    case "$errcode" in
        0)
            :
            ;;
        98|99)
            echo "Error calling deployment service"
            ;;
        *)
            return 1
    esac

    if echo "$upgrade_status_output" | grep 'upgraded to airgap bundle'; then
        upgrade_complete="true"
        break
    else
        echo "Retrying in 10 seconds"
        sleep 10
    fi
  done

  if [[ "$upgrade_complete" != "true" ]]; then
      echo "Services failed to upgrade in a reasonable amount of time."
      echo "Final upgrade status:"
      chef-automate upgrade status
      exit 1
  fi

  # TODO(ssd) 2018-10-17: Ugh. If an upgrade /just/ upgrades
  # configuration, it might take a while for all of the services to
  # get restarted by habitat, making our standard wait_for_healthy
  # function incorrect if it runs too soon.
  sleep 10
  wait_for_healthy
}

wait_for_healthy() {
  local healthy
  healthy="false"

  for try in {1..60}; do
    echo "Package upgrades complete, waiting for services to start... (try $try/60)"
    # shorten the timeout with retries to get more feed back during the deploy
    if chef-automate status -w -t 10; then
      healthy="true"
      break
    fi
  done

  if [[ "$healthy" != "true" ]]; then
    echo "Services failed report as healthy in a reasonable amount of time."
    echo "Final status:"
    chef-automate status -t 1
    exit 1
  fi
}

wait_for_install chef-automate
wait_for_install automate-backend-ctl
wait_for_frontend_aib
wait_for_backend_aib
wait_for_backend_ctl

mkdir -p /etc/chef-automate
timestamp=$(date +"%Y%m%d%H%M%S")
export timestamp

[ -e "/etc/chef-automate/config.toml" ] && cp -f /etc/chef-automate/config.toml /etc/chef-automate/config.toml.$timestamp
mv ${tmp_path}/automate_conf.toml /etc/chef-automate/config.toml
chmod 0600 /etc/chef-automate/config.toml*
rm ${automate_custom_config}

# Test if this is a non-bootstrap Automate or chef_api only install, else it's a bootstrap install
if [[ "${automate_role}" != "bootstrap_automate" ]]; then
  DEPLOY_BUNDLES="--airgap-bundle ${frontend_aib_file} --bootstrap-bundle ${tmp_path}/bootstrap.abb"
else
  DEPLOY_BUNDLES="--airgap-bundle ${frontend_aib_file}"
fi

if [ -e "/hab/user/deployment-service/config/user.toml" ]; then
  # existing installation
  echo "MAINTENANCE MODE ON!"
  if ! timeout 30 chef-automate maintenance on; then
    echo "ERROR while enabling maintance mode, this is likely caused by a configuration error"
    echo "Proceeding with update..."
  fi

  # hab symlink was replaced, move it to the side and let automate upgrade fix it
  [ ! -L /usr/bin/hab ] && mv /usr/bin/hab /usr/bin/hab.$timestamp

  echo "Upgrading Automate"

#  ERROR=$(chef-automate upgrade run --airgap-bundle ${frontend_aib_file} 2>&1 >/dev/null) || true
#  if echo "$ERROR" | grep 'This is a Major upgrade'; then
#    echo "y
#y
#y
#y
#y" | chef-automate upgrade run --major --airgap-bundle ${frontend_aib_file}

    # NOTE: This is a hack
    # The hack above was no longer good enough because we have a thing that needs
    # to be updated that isn't a service
#    sleep 45

    #shellcheck disable=SC2154
#    wait_for_upgrade
#    chef-automate post-major-upgrade migrate --data=PG -y
#  else
#    echo "regular normal upgrade airgap"
#    sleep 45

    #shellcheck disable=SC2154
#    wait_for_upgrade
#  fi
   chef-automate upgrade run --airgap-bundle ${frontend_aib_file}

   wait_for_upgrade

  echo "Applying /etc/chef-automate/config.toml"
  chef-automate config patch /etc/chef-automate/config.toml

  echo "MAINTENANCE MODE OFF"
  chef-automate maintenance off
else
  # we no longer need this hab but the one that chef-automate cli will install from the aib
  mv /usr/bin/hab /usr/bin/hab.$timestamp
  # Skip checks for the hab user as we create and manage that separately.
  # Fixes issues when the hab user/group is setup via LDAP in nsswitch configs.
  export CHEF_AUTOMATE_SKIP_HAB_USER=true
  chef-automate deploy /etc/chef-automate/config.toml $DEPLOY_BUNDLES --accept-terms-and-mlsa | grep --line-buffered -v "\┤\|\┘\|\└\|\┴\|\├\|\┌\|\┬\|\┴\|\┐"
fi

# actions to perform only on the Automate + bootstrap node
if [[ "${automate_role}" == "bootstrap_automate" ]]; then
  # reset the admin user password to the one specified in the TF config
  chef-automate iam admin-access restore '${admin_password}'
  # generate a bootstrap bundle and make it available to scp down
  rm -f ${tmp_path}/bootstrap.abb
  chef-automate bootstrap bundle create ${tmp_path}/bootstrap.abb
  chown ${ssh_user} ${tmp_path}/bootstrap.abb
fi

save_space
