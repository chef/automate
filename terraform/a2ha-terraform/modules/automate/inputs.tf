variable "airgap_info" {
}

variable "automate_admin_email" {
}

variable "automate_admin_password" {
}

variable "automate_admin_username" {
}

variable "automate_config" {
  default = ""
}

variable "automate_dc_token" {
}

variable "automate_fqdn" {
}

variable "automate_instance_count" {
  default = 1
}

variable "automate_license" {
  default = "Contact Chef Sales at sales@chef.io to request a license."
}

variable "automate_role" {
  description = "Set the type of server role. can be one of: bootstrap_automate, automate or chef_api"
}

variable "aws_os_snapshot_role_arn" {
  default = ""
}

variable "aws_region" {
  default     = "us-west-2"
  description = "The name of the selected AWS region / datacenter."
}

variable "backend_aib_dest_file" {
}

variable "backend_aib_local_file" {
}

variable "backup_config_efs" {
  default = "false"
}

variable "backup_config_s3" {
  default = "false"
}

variable "bucket_name" {
  default = "chef-automate-ha"
}

variable "cluster_id" {
  default = ""
}

variable "frontend_aib_dest_file" {
}

variable "frontend_aib_local_file" {
}

variable "hab_sup_http_gateway_auth_token" {
}

variable "habitat_info" {
}

variable "managed_opensearch_certificate" {
  default = ""
}

variable "managed_opensearch_domain_url" {
  default = ""
}

variable "managed_opensearch_user_password" {
  default = ""
}

variable "managed_opensearch_username" {
  default = ""
}

variable "managed_rds_certificate" {
  default = ""
}

variable "managed_rds_dbuser_password" {
  default = ""
}

variable "managed_rds_dbuser_username" {
  default = ""
}

variable "managed_rds_instance_url" {
  default = ""
}

variable "managed_rds_superuser_password" {
  default = ""
}

variable "managed_rds_superuser_username" {
  default = ""
}

variable "opensearch_listen_port" {
  default = 9200
}

variable "opensearch_private_ips" {
  default = []
}

variable "os_snapshot_user_access_key_id" {
  default = ""
}

variable "os_snapshot_user_access_key_secret" {
  default = ""
}

variable "postgresql_private_ips" {
  default = []
}

variable "postgresql_ssl_enable" {
}

variable "private_ips" {
  default = []
}

variable "proxy_listen_port" {
}

variable "public_ips" {
  default = []
}

variable "s3_endpoint" {
  default = "https://s3.amazonaws.com"
}

variable "setup_managed_services" {
  default = false
}

variable "ssh_key_file" {
}

variable "ssh_user" {
  default = "centos"
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "teams_port" {
}

variable "tmp_path" {
  default = "/var/automate-ha"
}