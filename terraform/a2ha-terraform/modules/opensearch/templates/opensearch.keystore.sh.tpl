#!/bin/bash

set -Eeuo pipefail

umask 0022

export HAB_NONINTERACTIVE=true
export HAB_NOCOLORING=true

OS_ORIGIN_NAME=$(echo "${opensearch_pkg_ident}" | awk -F/ '{print $1}')
export OS_ORIGIN_NAME
OS_PKG_NAME=$(echo "${opensearch_pkg_ident}" | awk -F/ '{print $2}')
export OS_PKG_NAME

# 
echo "${backup_config_s3}"

export HAB_LICENSE=accept-no-persist
SERVICE_UP_TIME=$(hab svc status  | awk  '{print $5}' | tail -1)


# Following loop will hold adding credentials in keystore until
# services are up and ran for 30 seconds and gives enough
# time to remove temp keystore file 'opensearch.keystore'

# If you get following error, ===> `[: : integer expression expected`
# try `until [[ "$SERVICE_UP_TIME" -gt 30 ]]` instead of `until [ "$SERVICE_UP_TIME" -gt 30 ]`
if [ "${backup_config_s3}" == "true" ]; then

max=15
n=0
until [ "$SERVICE_UP_TIME" -gt 30 -a $n -lt $max ]
do
  sleep 5
  SERVICE_UP_TIME=$(hab svc status  | awk  '{print $5}' | tail -1)
  echo "No services are loaded"
  echo "Sleeping for 5 seconds"
  n=$((n+1))
done

# Adding aws access and secret keys once all services are up

  echo "Setting up keystore"
  export OPENSEARCH_PATH_CONF="/hab/svc/automate-ha-opensearch/config"
  echo $OPENSEARCH_PATH_CONF
  echo "${access_key}" | hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore add --stdin --force s3.client.default.access_key
  echo "${secret_key}" | hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore add --stdin --force s3.client.default.secret_key
  hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore list
  sudo chown -RL hab:hab /hab/svc/automate-ha-opensearch/config/opensearch.keystore
  hab pkg exec "$OS_ORIGIN_NAME/$OS_PKG_NAME" opensearch-keystore list
  curl -k -X POST "https://127.0.0.1:${listen_port}/_nodes/reload_secure_settings?pretty" -u "${opensearch_username}:${opensearch_user_password}"
fi
