variable "automate-fqdn" {
  default = ""
}

variable "chef-server-fqdn" {
  default = ""
}

variable "chef_ips" {
  default = [] 
}

variable "private_ips" {
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