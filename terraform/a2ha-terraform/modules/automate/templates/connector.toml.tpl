[overrides]
  automate_fqdn = "${automate_fqdn}"
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
  [services.elasticsearch]
  ips = ${elasticsearch_ips}
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-elasticsearch"
  svc_port = ${elasticsearch_listen_port}
  managed_elasticsearch_certificate = "${managed_elasticsearch_certificate}"
  managed_elasticsearch_domain_url = "${managed_elasticsearch_domain_url}"
  managed_elasticsearch_user_password = "${managed_elasticsearch_user_password}"
  managed_elasticsearch_username = "${managed_elasticsearch_username}"

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
