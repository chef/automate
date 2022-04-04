#!/bin/bash

source /hab/sup/default/SystemdEnvironmentFile.sh
echo  " 
[es_yaml.path]      
  repo = /mnt/automate_backups/elasticsearch " > es_config.toml

hab config apply automate-ha-elasticsearch.default es_config.toml
