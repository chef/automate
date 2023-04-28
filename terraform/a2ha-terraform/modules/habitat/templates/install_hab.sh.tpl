#!/usr/bin/env bash

set -euo pipefail

umask 0022

export NO_SERVICE=false
export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true
export HAB_LICENSE=accept-no-persist

if [ "${install_hab_sh_args}" == "--no-service" ]; then
  export NO_SERVICE=true
fi

failure() {
  echo "$1"
  exit 1
}

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
  # this will truncate all but the most recent backend .aib files
  find /var/tmp/ -name backend\*aib -printf "%T+\t%p\n" | sort -r | awk '{print $2}' | tail -n +2 | xargs -n1 -I{} bash -c 'truncate_with_timestamp "$@"' _ {}
}

user_and_group() {
  # This brings the variable in from terraform and makes it available to bash
  habitat_uid_gid="${habitat_uid_gid}"

  if  id -u hab >/dev/null 2>&1; then
    echo "The hab user already exists."
  else
    %{ if habitat_uid_gid == "" }
    useradd -U hab
    # ensure hab group exists and is primary group for user hab
    id -gn hab 2>/dev/null | grep -q hab || { groupadd hab; usermod -g hab hab; }
    %{ else }
    groupadd -g $habitat_uid_gid hab && useradd -u $habitat_uid_gid -g $habitat_uid_gid hab
    # ensure hab group exists and is primary group for user hab
    id -gn hab 2>/dev/null | grep -q hab || { groupadd hab; usermod -g hab hab; }
    %{ endif }
  fi
}

wait_for_service_template() {
  max=20
  n=0
  until [ $n -ge $max ]; do
    cp -f ${tmp_path}/hab-sup.service /etc/systemd/system/hab-sup.service && break
    n=$((n+1))
    echo "Waiting for ${tmp_path}/hab-sup.service to be rendered.."
    sleep 30
  done
  if [[ $n -ge $max ]]; then
    failure "Failed waiting for ${tmp_path}/hab-sup.service to be rendered!"
  fi
}

wait_for_aib() {
  max=20
  n=0
  until [ $n -ge $max ]; do
    tar tf ${aib_file} >/dev/null 2>&1  && break
    n=$((n+1))
    file_stat=$(ls -l ${aib_file} 2>&1) || true
    echo "Waiting for transfer of ${aib_file} to complete: $file_stat"
    sleep 30
  done
  if [[ $n -ge $max ]]; then
    failure "Failed waiting for ${aib_file} to be transferred within $max iterations!"
  fi
}

setup_service() {
  cp -f "${tmp_path}/hab_peer_watch" /etc/hab_peer_watch
  mkdir -p /hab/sup/default
  # systemd ENV vars
cat << EOF > /hab/sup/default/SystemdEnvironmentFile.sh
#!/bin/bash

HAB_NONINTERACTIVE=true
HAB_NOCOLORING=true
HAB_LICENSE=accept-no-persist
HAB_SUP_GATEWAY_AUTH_TOKEN=${hab_sup_http_gateway_auth_token}
HAB_RING_KEY="${hab_sup_ring_key}"
export HAB_NONINTERACTIVE HAB_NOCOLORING HAB_LICENSE HAB_SUP_GATEWAY_AUTH_TOKEN HAB_RING_KEY
EOF
  chmod 700 /hab/sup/default/SystemdEnvironmentFile.sh
  # HTTP gateway TLS
cat << EOF > /hab/sup/default/HttpGatewayCA.pem
${hab_sup_http_gateway_ca_cert}
EOF
cat << EOF > /hab/sup/default/HttpGateway.key
${hab_sup_http_gateway_priv_key}
EOF
cat << EOF > /hab/sup/default/HttpGateway.pem
${hab_sup_http_gateway_pub_cert}
EOF
cat << EOF > /hab/sup/default/HttpGatewayChained.pem
${hab_sup_http_gateway_pub_cert}
${hab_sup_http_gateway_ca_cert}
EOF
  chmod 600 /hab/sup/default/HttpGateway.key
  chmod 664 /etc/systemd/system/hab-sup.service
  systemctl daemon-reload
  systemctl enable hab-sup.service
  systemctl start hab-sup.service
  # Habitat Supervisor starting up..
  sleep 5
}

user_and_group
wait_for_aib

[ -d "${tmp_path}" ] && rm -rf "${tmp_path}/aib_workspace"
mkdir -p "${tmp_path}/aib_workspace"
tar xf ${aib_file} -C ${tmp_path}/aib_workspace || failure "Failed to extract ${aib_file}!"
rsync -a --keep-dirlinks ${tmp_path}/aib_workspace/hab /

# Copy the hab bin if it doesn't exist
#[ ! -f /usr/bin/hab ] && rsync -a ${tmp_path}/aib_workspace/bin/hab /usr/bin
[ -f /usr/bin/hab ] && mv /usr/bin/hab /hab/var/
[ -L /usr/bin/hab ] && rm /usr/bin/hab 
ln -s ${tmp_path}/aib_workspace/bin/hab /usr/bin/hab
# TODO: remove this workaround once the following issue is resolved
# https://github.com/habitat-sh/habitat/issues/6260
export LOGCMD='>>${tmp_path}/svc-load.log 2>&1'

# For frontend and backend nodes
for pkg in /hab/cache/artifacts/{core-hab,*automate-ha-ctl,chef-automate-cli}*hart; do
  export pkg
  bash -c 'eval hab pkg install --force --binlink --binlink-dir /bin $pkg "$LOGCMD"' || true
done

if [ $NO_SERVICE = true ]; then
  touch "${aib_file}.DONE"
  save_space
  exit
fi

# Exclusively for backend nodes
for pkg in /hab/cache/artifacts/chef-automate-ha*hart; do
  export pkg
  bash -c 'eval hab pkg install --force --binlink --binlink-dir /bin $pkg "$LOGCMD"' || true
done

wait_for_service_template
setup_service

touch "${aib_file}.DONE"
save_space
