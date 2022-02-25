source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-backend-elasticsearch | tail -n +2 > es_config.toml

cat <<EOF >> es_config.toml
[es_yaml.path]
  repo = "$nfs_mount_path/elasticsearch"
EOF

hab config apply automate-backend-elasticsearch.default $(date '+%s') es_config.toml

