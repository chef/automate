variable "airgap_info" {
}

variable "backend_aib_dest_file" {
}

variable "backend_aib_local_file" {
}

variable "hab_sup_http_gateway_auth_token" {
}

variable "hab_sup_http_gateway_ca_cert" {
}

variable "hab_sup_http_gateway_priv_key" {
}

variable "hab_sup_http_gateway_pub_cert" {
}

variable "hab_sup_ring_key" {
}

variable "hab_sup_run_args" {
}

variable "habitat_uid_gid" {
  default = ""
}

variable "install_hab_sh_args" {
}

variable "instance_count" {
}

variable "peer_ips" {
  default = []
}

variable "private_ips" {
  default = []
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
