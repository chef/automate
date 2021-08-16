#!/usr/bin/env bash

export MINIO_LOG_FILE=/tmp/automate-MINIO.log

document "start_minio_background" <<DOC
  Start the minio in the background.

  To show the logs run the command:
    # minio_logs
DOC
function start_minio_background {
  log_line "Launching the $(green minio) in the background."
  setup_minio >> $MINIO_LOG_FILE 2>&1 &
  log_line "Run '$(yellow minio_logs)' to see how the automate-ui builds."
}

document "minio_logs" <<DOC
  Display the UI logs stored at '$UI_LOG_FILE'.
DOC
function minio_logs {
  tail -20f $MINIO_LOG_FILE
}

setup_minio(){
    rm -f minio
    wget https://dl.min.io/server/minio/release/linux-amd64/minio
    chmod +x minio
    ./minio server --console-address ":9565" /data
}
