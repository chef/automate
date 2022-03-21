variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "elasticsearch_private_ips" {
  default = []
}

variable "habitat_info" {
}

variable "instance_count" {
}

variable "metricbeat_module" {
  default = ""
}

variable "metricbeat_pkg_ident" {
}

variable "metricbeat_svc_binds" {
  default = ""
}

variable "metricbeat_svc_load_args" {
  default = "--strategy at-once"
}

variable "metricbeat_tags" {
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
