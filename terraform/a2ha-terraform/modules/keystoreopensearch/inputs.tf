variable "opensearch_instance_count" {
  default = 3
}

variable "secret_key" {
  default = ""
}
variable "access_key" {
  default = ""
}

variable "ssh_key_file" {
}

variable "ssh_port" {
  default = 22
}

variable "private_ips" {
  default = []
}

variable "opensearch_listen_port" {
  default = 9200
}

variable "opensearch_pkg_ident" {
  default = "chef/automate-ha-opensearch"
}

variable "ssh_user" {
  default = "ubuntu"
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tmp_path" {
  default = "/hab/var/automate-ha"
}

variable "backup_config_s3" {
  default = "false"
}
