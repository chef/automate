#!/usr/bin/env bash

export MINIO_LOG_FILE=/tmp/automate-MINIO.log

start_minio_background() {
  rm -f minio
  curl https://dl.min.io/server/minio/release/linux-amd64/minio -o minio
  chmod +x minio
  setup_minio >> $MINIO_LOG_FILE 2>&1 &
}

minio_logs() {
  tail -20f $MINIO_LOG_FILE
}

setup_minio(){
    ./minio server --console-address ":9565" /data
}

setup_minio_bucket(){
    rm -f mc
    curl https://dl.min.io/client/mc/release/linux-amd64/mc -o mc
    chmod +x mc
    ./mc alias set minio http://127.0.0.1:9000 minioadmin minioadmin
    ./mc mb minio/mybucket
}

wait_for_success() {
  local SECONDS_WAITING=${TIMEOUT:-60}
  local COUNTER=0

  until "$@" &> /dev/null; do
    sleep 1
    if [[ $COUNTER -ge "$SECONDS_WAITING" ]]; then
      return 1
    fi
    (( COUNTER=COUNTER+1 ))
  done
}
