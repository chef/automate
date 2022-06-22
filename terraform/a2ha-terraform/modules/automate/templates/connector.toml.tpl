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
  upgrade_strategy = "none"
  teams_port = ${teams_port}
  backup_config_s3 = "${backup_config_s3}"
  backup_config_efs = "${backup_config_efs}"
  bucket_name = "${bucket_name}"
  s3_endpoint = "${s3_endpoint}"

[services]
  [services.opensearch]
  ips = ${opensearch_ips}
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-opensearch"
  svc_port = ${opensearch_listen_port}
  managed_opensearch_certificate = "${managed_opensearch_certificate}"
  managed_opensearch_domain_url = "${managed_opensearch_domain_url}"
  managed_opensearch_user_password = "${managed_opensearch_user_password}"
  managed_opensearch_username = "${managed_opensearch_username}"
  aws_os_snapshot_role_arn = "${aws_os_snapshot_role_arn}"
  os_snapshot_user_access_key_id = "${os_snapshot_user_access_key_id}"
  os_snapshot_user_access_key_secret = "${os_snapshot_user_access_key_secret}"

  [services.postgresql]
  ips = ${postgresql_ips}
  managed_rds_certificate        = "${managed_rds_certificate}"
  managed_rds_dbuser_password    = "${managed_rds_dbuser_password}"
  managed_rds_dbuser_username    = "${managed_rds_dbuser_username}"
  managed_rds_instance_url       = "${managed_rds_instance_url}"
  managed_rds_superuser_password = "${managed_rds_superuser_password}"
  managed_rds_superuser_username = "${managed_rds_superuser_username}"
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-postgresql"
  svc_port = ${proxy_listen_port}
  ssl = ${postgresql_ssl_enable}
  setup_managed_services = ${setup_managed_services}
