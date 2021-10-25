variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "elasticsearch_private_ips" {
  default     = []
}

variable "habitat_info" {
}

variable "instance_count" {
}

variable "journalbeat_module" {
  default = ""
}

variable "journalbeat_pkg_ident" {
}

variable "journalbeat_svc_binds" {
  default = ""
}

variable "journalbeat_svc_load_args" {
  default = "--strategy at-once"
}

variable "journalbeat_tags" {
  default = []
}

variable "private_ips" {
  default     = []
}

variable "public_ips" {
  default = []
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

variable "tmp_path" {
  default = "/var/tmp"
}
