#!/usr/bin/env bash

export MINIO_LOG_FILE=/tmp/automate-MINIO.log

start_minio_background() {
  setup_minio >> $MINIO_LOG_FILE 2>&1 &
}

minio_logs() {
  tail -20f $MINIO_LOG_FILE
}

setup_minio(){
    rm -f minio
    wget https://dl.min.io/server/minio/release/linux-amd64/minio
    chmod +x minio
    ./minio server --console-address ":9565" /data
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
