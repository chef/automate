#!/bin/bash

source /hab/sup/default/SystemdEnvironmentFile.sh
<<EOT > es_config.toml
[path]      
  repo = "${opensearch_nfs_path}" ' 
EOT

hab config apply automate-ha-opensearch.default $(date '+%s') es_config.toml