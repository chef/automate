variable "automate_private_ips" {
  default = []
}

variable "chef_server_private_ips" {
  default = []
}

variable "elasticsearch_private_ips" {
  default = []
}

variable "postgresql_private_ips" {
  default = []
}

variable "public_subnets" {
  default = []
}

variable "private_subnets" {
  default = []
}

variable "ssh_key_file" {
}

variable "ssh_user" {
  default = "centos"
}

variable "ssh_user_sudo_password" {
    default = ""
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tmp_path" {
  default = "/var/tmp"
}

variable "tag_name" {
  default = "A2"
}

variable "tags" {
}

variable "aws_region" {
  default     = "us-west-1"
  description = "The name of the selected AWS region / datacenter."
}

variable "nfs_mount_path" {
  default = "/mnt/automate_backups"
}

variable "aws_vpc_id" {
}

variable "base_linux_aws_security_group_id" {
}

variable "aws_profile" {
  default     = "default"
  description = "The AWS profile to use from your ~/.aws/credentials file."
}

variable "aws_cluster_id" {
}

variable "efs_creation" {
  default = "false"
}

variable "private_custom_subnets"{
}
