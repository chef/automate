variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "curator_pkg_ident" {
}

variable "curator_svc_binds" {
  default = ""
}

variable "curator_svc_load_args" {
  default = "--strategy at-once"
}

variable "curator_tags" {
  default = []
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "habitat_info" {
}

variable "instance_count" {
}

variable "private_ips" {
  default     = []
  description = "the Elasticsearch Private IPs"
}

variable "public_ips" {
  default = []
}

variable "ssh_key_file" {
}

variable "ssh_user" {
  default = ""
}

variable "ssh_user_sudo_password" {
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tmp_path" {
  default = "/var/tmp"
}
