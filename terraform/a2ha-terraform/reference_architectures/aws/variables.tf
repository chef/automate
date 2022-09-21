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

variable "aws_os_snapshot_role_arn" {
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

variable "aws_s3_bucketName" {
  default = "chef-automate-ha"
}

variable "aws_ssh_key_pair_name" {
}

variable "aws_tags" {
}

variable "aws_vpc_id" {
}

variable "backup_config_efs" {
  default = "false"
}

variable "backup_config_s3" {
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

variable "delete_on_termination" {
  default = true
}

variable "destroy_bucket" {
  default = false
}

variable "managed_opensearch_certificate" {
  default = ""
}

variable "managed_opensearch_domain_name" {
  default = ""
}

variable "managed_opensearch_domain_url" {
  default = ""
}

variable "managed_opensearch_user_password" {
  default = ""
}

variable "managed_opensearch_username" {
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

variable "opensearch_ebs_volume_iops" {
  default = 300
}

variable "opensearch_ebs_volume_size" {
  default = 50
}

variable "opensearch_ebs_volume_type" {
  default = "gp3"
}

variable "opensearch_server_instance_type" {
  default = "m5a.large"
}

variable "os_snapshot_user_access_key_id" {
  default = ""
}

variable "os_snapshot_user_access_key_secret" {
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

variable "private_custom_subnets" {
  default = []
  type    = list(string)
}

variable "public_custom_subnets" {
  default = []
  type    = list(string)
}

variable "setup_managed_services" {
  default = false
}

variable "ssh_key_file" {
}

variable "ssh_port" {
  default = 22
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
