variable "backup_config_s3" {
  default = "false"
}

variable "backup_config_efs" {
  default = "false"
}

variable "access_key" {
  default = ""
}

variable "secret_key" {
  default = ""
}

variable "s3_endpoint" {
  default = "https://s3.amazonaws.com"
}

variable "bucket_name" {
  default = "chef-automate-ha"
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
