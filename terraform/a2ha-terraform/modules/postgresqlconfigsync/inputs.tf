variable "postgresql_certs_by_ip" {
  type    = map(map(string))
  default = {}
}

variable "postgresql_instance_count" {
  default = 3
}


variable "private_ips" {
  default = []
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
