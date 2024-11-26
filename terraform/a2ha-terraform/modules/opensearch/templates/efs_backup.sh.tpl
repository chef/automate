#!/bin/bash

source /hab/sup/default/SystemdEnvironmentFile.sh
<<EOT > es_config.toml
[path]      
  repo = "${nfs_mount_path}/${opensearch_base_path}" ' 
EOT

hab config apply automate-ha-opensearch.default $(date '+%s') es_config.toml