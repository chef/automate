[Service]
ExecStart=
ExecStart=/bin/hab sup run \
  --event-stream-application="load-generation" \
  --event-stream-environment="pipeline" \
  --event-stream-site="chef-cd-pipeline" \
  --event-stream-url="${automate_server_fqdn}:4222" \
  --event-stream-token="${automate_server_token}"

