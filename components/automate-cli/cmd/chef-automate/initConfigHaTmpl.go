package main

const haAwsConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.aws]
# ============== Access & Backup Config =================
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "aws"
workspace_path = "/hab/a2_deploy_workspace"
# ssh user name for ssh login to instance like default user for centos will centos or for red-hat will be ec2-user
ssh_user = "centos"
# private ssh key file path to access instances
ssh_key_file = "~/.ssh/A2HA.pem"
# sudo_password = ""
# Backup config type can be efs and s3
backup_config = ""
# If s3 is selected for backup_config ,then uncomment and give s3_bucketName or else default chef-automate-ha.<deployment-string> will go
# s3_bucketName = "chef-automate-ha"

# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"
# ======================================================

# ============== EC2 Nodes Config ======================
# Please use the https://docs.chef.io/calculator/automate_ha_hardware_calculator.xlsx to calculate, No and type of instance needed.

[automate.config]
# admin_password = ""
# automate load balancer fqdn IP or path
# fqdn = ""
#Deprecated Config - automate_setup_type is not supported
# automate_setup_type = "automate"
instance_count = "1"
# teams_port = ""
config_file = "configs/automate.toml"

[chef_server.config]
instance_count = "1"

[opensearch.config]
instance_count = "3"

[postgresql.config]
instance_count = "3"
# ======================================================

[aws.config]
# ============== AWS network Config ============================
profile = "default"
region = "us-east-1"
# Provide vpcid and cidr block
# E.g. aws_vpc_id = "vpc12318h"
# E.g. aws_cidr_block_addr = "172.31.64.0"
aws_vpc_id  = ""
aws_cidr_block_addr  = ""
private_custom_subnets = []
public_custom_subnets = []
# ssh key pair name in AWS to access instances
ssh_key_pair_name = "A2HA"
# ======================================================

# ============== Managed Services ======================
# in case your are trying to deploy with aws managed 
# RDS, and openseach, then make setup_managed_services = true, 
# and modify other managed services settings.
setup_managed_services = false
managed_opensearch_domain_name = ""
managed_opensearch_domain_url = ""
managed_opensearch_username = ""
managed_opensearch_user_password = ""
managed_opensearch_certificate = ""
aws_os_snapshot_role_arn = ""
os_snapshot_user_access_key_id = ""
os_snapshot_user_access_key_secret = ""
managed_rds_instance_url = ""
managed_rds_superuser_username = ""
managed_rds_superuser_password = ""
managed_rds_dbuser_username = ""
managed_rds_dbuser_password = ""
managed_rds_certificate = ""
# ======================================================

# ============== EC2 Instance Config ===================
ami_filter_name = ""
ami_filter_virt_type = ""
ami_filter_owner = ""
ami_id = ""
# Enabale/Disable load balancer logs
lb_access_logs = "false"
automate_server_instance_type = "t3.medium"
chef_server_instance_type = "t3.medium"
opensearch_server_instance_type = "m5.large"
postgresql_server_instance_type = "t3.medium"
automate_lb_certificate_arn = "arn:aws:acm...."
chef_server_lb_certificate_arn = "arn:aws:acm...."
automate_ebs_volume_iops = "100"
automate_ebs_volume_size = "50"
automate_ebs_volume_type = "gp3"
chef_ebs_volume_iops = "100"
chef_ebs_volume_size = "50"
chef_ebs_volume_type = "gp3"
opensearch_ebs_volume_iops = "100"
opensearch_ebs_volume_size = "50"
opensearch_ebs_volume_type = "gp3"
postgresql_ebs_volume_iops = "100"
postgresql_ebs_volume_size = "50"
postgresql_ebs_volume_type = "gp3"
# ======================================================

# ============== EC2 Instance Tags =====================
X-Contact = ""
X-Dept = ""
X-Project = ""
# ======================================================

# ============== Deprecated ============================
#Deprecated Config - below config is not supported
#aws_automate_route53_prefix = ""
#aws_chef_server_route53_prefix = ""
#aws_route53_hosted_zone = "saas.chef.io"
#postgresql_db_identifier = ""
#elasticsearch_domain_name = ""
#rds_postgresql_instance_type = "db.t3.medium"
#rds_postgresql_restore_identifier = ""
#datadog_api_key = "DATADOG_API_KEY"
#use_existing_managed_infra = false
#X-Production = "false"
#X-Customer = ""
# ======================================================
`

const haExistingNodesConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.existing_infra]
# ============== Access & Backup Config =================
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "existing_nodes"
workspace_path = "/hab/a2_deploy_workspace"
ssh_user = "centos"
# private ssh key file path to access instances
ssh_key_file = "~/.ssh/A2HA.pem"
sudo_password = ""

# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"
# ======================================================

# ============== EC2 Nodes Config ======================
[automate.config]
# admin_password = ""
# automate load balancer fqdn IP or path
fqdn = ""
instance_count = "1"
# teams_port = ""
config_file = "configs/automate.toml"

[chef_server.config]
instance_count = "1"

[opensearch.config]
instance_count = "3"

[postgresql.config]
instance_count = "3"

[existing_infra.config]
# provide comma seperated ip address of nodes, like ["192.0.0.1", "192.0.0.2", "192.0.0.2"]
# No of ip address should be same as No of instance_count count mentioned above in 
# automate.config, chef_server.config, opensearch.config and postgresql.config
automate_private_ips = []
chef_server_private_ips = []
opensearch_private_ips = []
postgresql_private_ips = []
# ======================================================
`

var UsageTemplate string = `

Usage:
  chef-automate init-config-ha [arg] or [flag]

Flags:
      --file string               File path to write the config (default "config.toml")
  -h, --help                      help for init-config-ha

Args: 
  aws				Generate initial automate high availability configuration for AWS deployment
  existing_infra		Generate initial automate high availability configuration for existing infra nodes deployment

Global Flags:
  -d, --debug                Enable debug output
      --no-check-version     Disable version check
      --result-json string   Write command result as JSON to PATH	  
`
