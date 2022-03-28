variable "ip_list" {
  default = []
}

variable "automate_private_ips" {
  default = []
}

variable "chef_server_private_ips" {
  default = []
}

variable "elasticsearch_private_ips" {
  default = []
}

variable "elasticsearch_public_ips" {
  default = []
}

variable "postgresql_private_ips" {
  default = []
}

variable "aws_ssh_key_file" {
}

variable "aws_ssh_user" {
  default = "centos"
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "aws_region" {
  default     = "us-west-2"
  description = "The name of the selected AWS region / datacenter."
}

variable "aws_ssh_key_pair_name" {
}

variable "tag_name" {
}

variable "ssh_user_sudo_password" {
  default = ""
}

variable "random_id" {
}

#variable "tags" {
#}

variable "nfs_mount_path" {
  default = "/mnt/automate_backups"
}

variable "tmp_path" {
  default = "/var/tmp"
}

variable "mount_id" {
}

variable "subnet_id" {
  default = []
}
