variable "ami_filter_name" {
  default = "chef-highperf-centos7-*"
}

variable "ami_filter_owner" {
  default = "446539779517"
}

variable "ami_filter_virt_type" {
  default = "hvm"
}

variable "automate_certs_by_ip" {
  type = map(map(string))
  default = {}
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

variable "automate_private_ips" {
  default = []
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

variable "aws_s3_bucketName" {
  default = "chef-automate-ha"
}

variable "bucket_name_deployment" {
  default = "chef-automate-ha"
}

variable "aws_os_snapshot_role_arn" {
  default = ""
}

variable "aws_os_snapshot_role_arn_deployment" {
  default = ""
}

variable "aws_profile" {
  default     = "default"
  description = "The AWS profile to use from your ~/.aws/credentials file."
}

variable "aws_region" {
  default     = ""
  description = "The name of the selected AWS region / datacenter."
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

variable "bucket_name" {
}

variable "chef_server_certs_by_ip" {
  type = map(map(string))
  default = {}
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

variable "chef_server_private_ips" {
  default = []
}

variable "delete_on_termination" {
  default = true
}

variable "managed_opensearch_certificate" {
  default = ""
}

variable "managed_opensearch_certificate_deployment" {
  default = ""
}

variable "managed_opensearch_domain_name" {
  default = ""
}

variable "managed_opensearch_domain_url" {
  default = ""
}

variable "managed_opensearch_domain_url_deployment" {
  default = ""
}

variable "managed_opensearch_user_password" {
  default = ""
}

variable "managed_opensearch_username" {
  default = ""
}

variable "managed_opensearch_username_deployment" {
  default = ""
}

variable "managed_rds_certificate_deployment" {
  default = ""
}

variable "managed_rds_dbuser_password" {
  default = ""
}

variable "managed_rds_dbuser_password_deployment" {
  default = ""
}

variable "managed_rds_dbuser_username" {
  default = ""
}

variable "managed_rds_dbuser_username_deployment" {
  default = ""
}

variable "managed_rds_instance_url" {
  default = ""
}

variable "managed_rds_instance_url_deployment" {
  default = ""
}

variable "managed_rds_superuser_password" {
  default = ""
}

variable "managed_rds_superuser_password_deployment" {
  default = ""
}

variable "managed_rds_superuser_username" {
  default = ""
}

variable "managed_rds_superuser_username_deployment" {
  default = ""
}

variable "opensearch_certs_by_ip" {
  type = map(map(string))
  default = {}
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

variable "opensearch_private_ips" {
  default = []
}

variable "opensearch_root_cert" {
    default = ""
}

variable "opensearch_server_instance_type" {
  default = "m5a.large"
}

variable "os_snapshot_user_access_key_id" {
  default = ""
}

variable "os_snapshot_user_access_key_id_deployment" {
  default = ""
}

variable "os_snapshot_user_access_key_secret" {
  default = ""
}

variable "os_snapshot_user_access_key_secret_deployment" {
  default = ""
}

variable "postgresql_certs_by_ip" {
  type = map(map(string))
  default = {}
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

variable "postgresql_private_ips" {
  default = []
}

variable "postgresql_root_cert" {
  default = ""
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

variable "s3_endpoint" {
  default = "https://s3.amazonaws.com"
}

variable "setup_managed_services" {
  default = false
}

variable "setup_self_managed_services" {
  default = false
}

variable "ssh_group_name" {
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
