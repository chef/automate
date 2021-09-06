variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "habitat_info" {
}

variable "instance_count" {
}

variable "kibana_pkg_ident" {
}

variable "kibana_svc_binds" {
  default = ""
}

variable "kibana_svc_load_args" {
  default = "--strategy at-once"
}

variable "kibana_tags" {
  default = []
}

variable "nginx_elasticsearch_auth" {
  default = "admin"
}

variable "nginx_elasticsearch_user" {
  default = "admin"
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
