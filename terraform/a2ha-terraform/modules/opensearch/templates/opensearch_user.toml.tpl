[network]
  port = ${listen_port}
  host = "${private_ip}"

[discovery]
  minimum_master_nodes = ${minimum_masters}
  ping_unicast_hosts = [ ${private_ips} ]

[transport]
  port = 9300

${ "${backup_config}" == "s3" ? <<EOT
[s3]
  [s3.client.default]
    endpoint = "${endpoint}"
    protocol = "${protocol}"

EOT 
: "${backup_config}" == "efs" ? <<EOT
[path]      
  repo = "${nfs_mount_path}/opensearch"
EOT 
: "" }

${ "${opensearch_custom_certs_enabled}" == true ? <<EOT
[tls]
  rootCA = """${opensearch_root_ca}"""
  admin_cert = """${opensearch_admin_cert}"""
  admin_key = """${opensearch_admin_key}"""
  ssl_cert = """${opensearch_public_key}"""
  ssl_key = """${opensearch_private_key}"""

[plugins.security.authcz]
admin_dn = "- ${opensearch_admin_dn}"
[plugins.security.ssl.transport]
enforce_hostname_verification = false
resolve_hostname = false
[plugins.security]
nodes_dn = """
- ${opensearch_nodes_dn}
"""
EOT
: "" }
