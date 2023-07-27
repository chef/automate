[overrides]
  automate_fqdn = "${automate_fqdn}"
  aws_region = "${aws_region}"
  automate_admin_email = "${automate_admin_email}"
  automate_admin_username = "${automate_admin_username}"
  automate_admin_password = "${automate_admin_password}"
  automate_config_file = "${automate_custom_config}"
  automate_dc_token = "${automate_dc_token}"
  automate_role = "${automate_role}"
  channel = "current"
  setup_managed_services = ${setup_managed_services}
  setup_self_managed_services = ${setup_self_managed_services}
  upgrade_strategy = "none"
  teams_port = ${teams_port}
  backup_config_s3 = "${backup_config_s3}"
  backup_config_efs = "${backup_config_efs}"
  nfs_mount_path = "${nfs_mount_path}"
  bucket_name = "${bucket_name}"
  s3_endpoint = "${s3_endpoint}"
  access_key = "${access_key}"
  secret_key = "${secret_key}"
  infra = "${infra}"
  automate_root_ca = """${automate_root_ca}"""
  automate_private_key = """${automate_private_key}"""
  automate_public_key = """${automate_public_key}"""
  chef_server_private_key = """${chef_server_private_key}"""
  chef_server_public_key = """${chef_server_public_key}"""
  opensearch_root_ca = """${opensearch_root_ca}"""
  postgresql_root_ca = """${postgresql_root_ca}"""
  automate_custom_certs_enabled = ${automate_custom_certs_enabled}
  chef_server_custom_certs_enabled = ${chef_server_custom_certs_enabled}
  postgresql_custom_certs_enabled = ${postgresql_custom_certs_enabled}
  opensearch_custom_certs_enabled = ${opensearch_custom_certs_enabled}

  
[services]
  [services.opensearch]
  ips = ${opensearch_ips}
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-opensearch"
  svc_port = ${opensearch_listen_port}
  managed_opensearch_domain_name = "${managed_opensearch_domain_name}"
  managed_opensearch_certificate = """${managed_opensearch_certificate}"""
  managed_opensearch_domain_url = "${managed_opensearch_domain_url}"
  managed_opensearch_user_password = "${managed_opensearch_user_password}"
  managed_opensearch_username = "${managed_opensearch_username}"
  aws_os_snapshot_role_arn = "${aws_os_snapshot_role_arn}"
  os_snapshot_user_access_key_id = "${os_snapshot_user_access_key_id}"
  os_snapshot_user_access_key_secret = "${os_snapshot_user_access_key_secret}"
  opensearch_root_cert = """${opensearch_root_cert}"""

  [services.postgresql]
  ips = ${postgresql_ips}
  managed_rds_certificate        = """${managed_rds_certificate}"""
  managed_rds_dbuser_password    = "${managed_rds_dbuser_password}"
  managed_rds_dbuser_username    = "${managed_rds_dbuser_username}"
  managed_rds_instance_url       = "${managed_rds_instance_url}"
  managed_rds_superuser_password = "${managed_rds_superuser_password}"
  managed_rds_superuser_username = "${managed_rds_superuser_username}"
  postgresql_root_cert           = """${postgresql_root_cert}"""
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-postgresql"
  svc_port = ${proxy_listen_port}
  ssl = ${postgresql_ssl_enable}
  setup_managed_services = ${setup_managed_services}
