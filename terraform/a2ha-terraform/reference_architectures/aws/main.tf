data "http" "getEc2PrivateIP" {
  url = "http://169.254.169.254/latest/meta-data/local-ipv4"
}

locals {
  json_data = "${data.http.getEc2PrivateIP.response_body}/32"
}
provider "aws" {
  region  = var.aws_region
  profile = var.aws_profile
}

module "aws" {
  ami_filter_name                    = var.ami_filter_name
  ami_filter_owner                   = var.ami_filter_owner
  ami_filter_virt_type               = var.ami_filter_virt_type
  aws_region                         = var.aws_region
  aws_vpc_id                         = var.aws_vpc_id
  aws_cidr_block_addr                = var.aws_cidr_block_addr
  automate_ebs_volume_iops           = var.automate_ebs_volume_iops
  automate_ebs_volume_size           = var.automate_ebs_volume_size
  automate_ebs_volume_type           = var.automate_ebs_volume_type
  automate_root_ebs_volume_iops      = var.automate_root_ebs_volume_iops
  automate_root_ebs_volume_size      = var.automate_root_ebs_volume_size
  automate_root_ebs_volume_type      = var.automate_root_ebs_volume_type
  automate_fqdn                      = var.automate_fqdn
  automate_instance_count            = var.automate_instance_count
  automate_lb_certificate_arn        = var.automate_lb_certificate_arn
  delete_on_termination              = var.delete_on_termination
  automate_server_instance_type      = var.automate_server_instance_type
  aws_ami_id                         = var.aws_ami_id
  aws_ssh_key_file                   = var.ssh_key_file
  aws_ssh_key_pair_name              = var.aws_ssh_key_pair_name
  aws_ssh_user                       = var.ssh_user
  aws_ssh_port                       = var.ssh_port
  chef_ebs_volume_iops               = var.automate_ebs_volume_iops
  chef_ebs_volume_size               = var.automate_ebs_volume_size
  chef_ebs_volume_type               = var.automate_ebs_volume_type
  chef_root_ebs_volume_iops          = var.automate_root_ebs_volume_iops
  chef_root_ebs_volume_size          = var.automate_root_ebs_volume_size
  chef_root_ebs_volume_type          = var.automate_root_ebs_volume_type
  chef_server_instance_count         = var.chef_server_instance_count
  chef_server_instance_type          = var.chef_server_instance_type
  chef_server_lb_certificate_arn     = var.chef_server_lb_certificate_arn
  public_custom_subnets              = var.public_custom_subnets
  private_custom_subnets             = var.private_custom_subnets
  opensearch_ebs_volume_iops         = var.opensearch_ebs_volume_iops
  opensearch_ebs_volume_size         = var.opensearch_ebs_volume_size
  opensearch_ebs_volume_type         = var.opensearch_ebs_volume_type
  opensearch_root_ebs_volume_iops    = var.opensearch_root_ebs_volume_iops
  opensearch_root_ebs_volume_size    = var.opensearch_root_ebs_volume_size
  opensearch_root_ebs_volume_type    = var.opensearch_root_ebs_volume_type
  opensearch_instance_count          = var.opensearch_instance_count
  opensearch_listen_port             = var.opensearch_listen_port
  opensearch_server_instance_type    = var.opensearch_server_instance_type
  managed_opensearch_domain_name     = var.managed_opensearch_domain_name
  managed_opensearch_domain_url      = var.managed_opensearch_domain_url
  aws_os_snapshot_role_arn           = var.aws_os_snapshot_role_arn
  os_snapshot_user_access_key_id     = var.os_snapshot_user_access_key_id
  os_snapshot_user_access_key_secret = var.os_snapshot_user_access_key_secret
  pgleaderchk_listen_port            = var.pgleaderchk_listen_port
  postgresql_ebs_volume_iops         = var.postgresql_ebs_volume_iops
  postgresql_ebs_volume_size         = var.postgresql_ebs_volume_size
  postgresql_ebs_volume_type         = var.postgresql_ebs_volume_type
  postgresql_root_ebs_volume_iops    = var.postgresql_root_ebs_volume_iops
  postgresql_root_ebs_volume_size    = var.postgresql_root_ebs_volume_size
  postgresql_root_ebs_volume_type    = var.postgresql_root_ebs_volume_type
  postgresql_instance_count          = var.postgresql_instance_count
  postgresql_listen_port             = var.postgresql_listen_port
  postgresql_server_instance_type    = var.postgresql_server_instance_type
  proxy_listen_port                  = var.proxy_listen_port
  setup_managed_services             = var.setup_managed_services
  source                             = "./modules/aws"
  lb_access_logs                     = var.lb_access_logs
  tags                               = var.aws_tags
  aws_instance_profile_name          = var.backup_config_s3 == "true" ? module.s3[0].instance_profile_name : null
  json_data                          = local.json_data
}

module "efs" {
  source                  = "./modules/efs"
  count                   = var.backup_config_efs == "true" ? 1 : 0
  automate_private_ips    = module.aws.automate_private_ips
  chef_server_private_ips = module.aws.chef_server_private_ips
  postgresql_private_ips  = module.aws.postgresql_private_ips
  opensearch_private_ips  = module.aws.opensearch_private_ips
  random_id               = module.aws.random_id
  aws_region              = var.aws_region
  aws_ssh_key_file        = var.ssh_key_file
  aws_ssh_key_pair_name   = var.aws_ssh_key_pair_name
  aws_ssh_user            = var.ssh_user
  aws_ssh_port            = var.ssh_port
  tag_name                = var.tag_name
  subnet_id               = module.aws.subnet_id
  mount_id                = module.aws.mount_id

  depends_on = [module.aws]
}

module "s3" {
  source            = "./modules/s3"
  count             = var.backup_config_s3 == "true" ? 1 : 0
  aws_s3_bucketName = var.aws_s3_bucketName
  random_id         = module.aws.random_id
  tags              = var.tag_name
  destroy_bucket    = var.destroy_bucket
}

module "aws-output" {
  source                             = "./modules/aws_output"
  automate_private_ips               = module.aws.automate_private_ips
  chef_server_private_ips            = module.aws.chef_server_private_ips
  postgresql_private_ips             = module.aws.postgresql_private_ips
  opensearch_public_ips              = module.aws.opensearch_public_ips
  opensearch_private_ips             = module.aws.opensearch_private_ips
  automate_fqdn                      = module.aws.automate_fqdn
  automate_frontend_urls             = module.aws.automate_frontend_urls
  bucket_name                        = var.backup_config_s3 == "true" ? module.s3[0].bucket_name : ""
  aws_os_snapshot_role_arn           = module.aws.aws_os_snapshot_role_arn
  os_snapshot_user_access_key_id     = module.aws.os_snapshot_user_access_key_id
  os_snapshot_user_access_key_secret = module.aws.os_snapshot_user_access_key_secret
}
