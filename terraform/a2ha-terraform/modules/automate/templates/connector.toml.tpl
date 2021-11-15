[overrides]
  automate_fqdn = "${automate_fqdn}"
  automate_admin_email = "${automate_admin_email}"
  automate_admin_username = "${automate_admin_username}"
  automate_admin_password = "${automate_admin_password}"
  automate_config_file = "${automate_custom_config}"
  automate_dc_token = "${automate_dc_token}"
  automate_role = "${automate_role}"
  channel = "current"
  upgrade_strategy = "none"
  teams_port = ${teams_port}

[services]
  [services.elasticsearch]
  ips = ${elasticsearch_ips}
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-elasticsearch"
  svc_port = ${elasticsearch_listen_port}

  [services.postgresql]
  ips = ${postgresql_ips}
  sup_port = 9631
  svc_group = "default"
  svc_name = "automate-ha-postgresql"
  svc_port = ${proxy_listen_port}
  ssl = ${postgresql_ssl_enable}
