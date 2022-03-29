variable "ami_filter_name" {
  default = "chef-highperf-centos7-*"
}

variable "ami_filter_owner" {
  default = "446539779517"
}

variable "ami_filter_virt_type" {
  default = "hvm"
}

variable "automate_ebs_volume_iops" {
  default = 100
}

variable "automate_ebs_volume_size" {
  default = 50
}

variable "automate_ebs_volume_type" {
  default = "gp3"
}

variable "automate_fqdn" {
}

variable "automate_instance_count" {
  default = 1
}

variable "automate_lb_certificate_arn" {
}

variable "automate_server_instance_type" {
  default = "t3a.medium"
}

variable "aws_ami_id" {
  default     = ""
  description = "Setting this value overrides ami search features"
}

variable "aws_cidr_block_addr" {
}

variable "aws_instance_profile_name" {
  default = ""
}

variable "aws_profile" {
  default     = "default"
  description = "The AWS profile to use from your ~/.aws/credentials file."
}

variable "aws_region" {
  default     = "us-west-1"
  description = "The name of the selected AWS region / datacenter."
}

variable "aws_ssh_key_file" {
}

variable "aws_ssh_key_pair_name" {
}

variable "aws_ssh_user" {
  default = "centos"
}

variable "aws_vpc_id" {
}

variable "chef_ebs_volume_iops" {
  default = 100
}

variable "chef_ebs_volume_size" {
  default = 50
}

variable "chef_ebs_volume_type" {
  default = "gp3"
}

variable "chef_server_instance_count" {
  default = 1
}

variable "chef_server_instance_type" {
  default = "t3a.medium"
}

variable "chef_server_lb_certificate_arn" {
}

variable "elasticsearch_ebs_volume_iops" {
  default = 300
}

variable "elasticsearch_ebs_volume_size" {
  default = 50
}

variable "elasticsearch_ebs_volume_type" {
  default = "gp3"
}

variable "elasticsearch_instance_count" {
  default = 3
}

variable "elasticsearch_listen_port" {
  default = 9200
}

variable "elasticsearch_server_instance_type" {
  default = "m5a.large"
}

variable "kibana_listen_port" {
  default = 5601
}

variable "lb_access_logs" {
  default = false
}

variable "nfs_mount_path" {
  default = "/mnt/automate_backups"
}

variable "pgleaderchk_listen_port" {
  default = 6432
}

variable "postgresql_ebs_volume_iops" {
  default = 150
}

variable "postgresql_ebs_volume_size" {
  default = 50
}

variable "postgresql_ebs_volume_type" {
  default = "gp3"
}

variable "postgresql_instance_count" {
  default = 3
}

variable "postgresql_listen_port" {
  default = 5432
}

variable "postgresql_server_instance_type" {
  default = "t3a.medium"
}

variable "proxy_listen_port" {
  default = 7432
}

variable "setup_managed_services" {
  default = false
}

variable "ssh_user_sudo_password" {
  default = ""
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tag_name" {
  default = "A2"
}

variable "tags" {
}

variable "tmp_path" {
  default = "/var/tmp"
}