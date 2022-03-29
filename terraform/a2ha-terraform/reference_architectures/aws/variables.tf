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

variable "automate_lb_certificate_arn" {
  default = "arn:aws:acm:us-west-2:446539779517:certificate/e98235a7-ba3d-4900-9c55-4b35bb8b56c7"
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
  default     = "us-west-2"
  description = "The name of the selected AWS region / datacenter."
}

variable "aws_ssh_key_pair_name" {
}

variable "aws_tags" {
}

variable "aws_vpc_id" {
}

variable "bucket_config_efs" {
  default = "false"
}

variable "bucket_config_s3" {
  default = "false"
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

variable "chef_server_instance_type" {
  default = "t3a.medium"
}

variable "chef_server_lb_certificate_arn" {
  default = "arn:aws:acm:us-west-2:446539779517:certificate/e98235a7-ba3d-4900-9c55-4b35bb8b56c7"
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

variable "elasticsearch_server_instance_type" {
  default = "m5a.large"
}

variable "managed_elasticsearch_certificate" {
  default = ""
}

variable "managed_elasticsearch_domain_url" {
  default = ""
}

variable "managed_elasticsearch_user_password" {
  default = ""
}

variable "managed_elasticsearch_username" {
  default = ""
}

variable "managed_rds_certificate" {
  default = ""
}

variable "managed_rds_dbuser_password" {
  default = ""
}

variable "managed_rds_dbuser_username" {
  default = ""
}

variable "managed_rds_instance_url" {
  default = ""
}

variable "managed_rds_superuser_password" {
  default = ""
}

variable "managed_rds_superuser_username" {
  default = ""
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

variable "postgresql_server_instance_type" {
  default = "t3a.medium"
}

variable "setup_managed_services" {
  default = false
}

variable "ssh_key_file" {
}

variable "ssh_user" {
  default = "centos"
}

variable "sudo_cmd" {
  default = "sudo"
}

variable "tag_name" {
  default = "A2"
}