[network]
  port = ${listen_port}
  host = "${private_ip}"

[discovery]
  minimum_master_nodes = ${minimum_masters}
  ping_unicast_hosts = [ ${private_ips} ]

[transport]
  port = 9300


# "backup_config_efs: ${backup_config_efs}"
# "backup_config_s3: ${backup_config_s3}"

${ "${backup_config_s3}" == "true" ? <<EOT
[s3]
  [s3.client.default]
    endpoint = "${endpoint}"

EOT 
: "" }

