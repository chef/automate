variable "pgleaderchk_listen_port" {
  default = 6432
}

variable "pgleaderchk_pkg_ident" {
}

variable "pgleaderchk_svc_load_args" {
}

variable "postgresql_certs_by_ip" {
  type    = map(map(string))
  default = {}
}

variable "postgresql_instance_count" {
  default = 3
}

variable "postgresql_listen_port" {
  default = 5432
}

variable "postgresql_pkg_ident" {
}


variable "postgresql_svc_load_args" {
}

variable "private_ips" {
  default = []
}

variable "proxy_listen_port" {
  default = 7432
}

variable "proxy_pkg_ident" {
}

variable "proxy_svc_load_args" {
}

variable "public_ips" {
  default = []
}

variable "ssh_key_file" {
}

variable "ssh_port" {
  default = 22
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
  default = "/hab/var/automate-ha"
}
