variable "access_key" {
  default = ""
}

variable "backup_config_efs" {
  default = "false"
}

variable "backup_config_s3" {
  default = "false"
}

variable "bucket_name" {
  default = ""
}

variable "existing_automate_ips" {
  default = []
}

variable "existing_automate_private_ips" {
  default = []
}

variable "existing_chef_server_ips" {
  default = []
}

variable "existing_chef_server_private_ips" {
  default = []
}

variable "existing_opensearch_ips" {
  default = []
}

variable "existing_opensearch_private_ips" {
  default = []
}

variable "existing_postgresql_ips" {
  default = []
}

variable "existing_postgresql_private_ips" {
  default = []
}

variable "infra" {
  default = "existing_infra"
}

variable "region" {
  default = ""
}

variable "s3_endpoint" {
  default = "https://s3.amazonaws.com"
}

variable "secret_key" {
  default = ""
}

variable "ssh_key_file" {
}

variable "ssh_port" {
}

variable "ssh_user" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tag_contact" {
  default = ""
}

variable "tag_dept" {
  default = ""
}

variable "tag_name" {
  default = "A2"
}

variable "tag_project" {
  default = ""
}

variable "managed_opensearch_certificate" {
  default = ""
}

variable "managed_opensearch_domain_name" {
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

variable "setup_managed_services" {
  default = false
}

variable "os_snapshot_user_access_key_id" {
  default = ""
}

variable "os_snapshot_user_access_key_secret" {
  default = ""
}

variable "aws_os_snapshot_role_arn" {
  default = ""
}