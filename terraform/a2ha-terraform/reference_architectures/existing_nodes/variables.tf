variable "existing_automate_ips" {
  default = []
}

variable "existing_automate_private_ips" {
  default = []
}

variable "existing_chef_server_fqdn" {
  default = []
}

variable "existing_chef_server_ips" {
  default = []
}

variable "existing_chef_server_private_ips" {
  default = []
}

variable "existing_elasticsearch_ips" {
  default = []
}

variable "existing_elasticsearch_private_ips" {
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