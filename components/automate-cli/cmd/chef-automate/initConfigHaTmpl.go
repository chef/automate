package main

const haAwsConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.aws]
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "aws"
workspace_path = "/hab/a2_deploy_workspace"
# ssh user name for ssh login to instance like default user for centos will centos or for red-hat will be ec2-user
ssh_user = "centos"
# private ssh key file path to access instances
ssh_key_file = "~/.ssh/A2HA.pem"
# sudo_password = ""

# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"

[automate.config]
# admin_password = ""
# automate load balancer fqdn IP or path
# fqdn = ""
instance_count = "1"
# teams_port = ""
config_file = "configs/automate.toml"

[chef_server.config]
instance_count = "1"

[elasticsearch.config]
instance_count = "3"

[postgresql.config]
instance_count = "3"

[aws.config]
profile = "default"
region = "us-east-1"
# Provide vpcid and cidr block
# E.g. aws_vpc_id = "vpc12318h"
# E.g. aws_cidr_block_addr = "172.31.64.0"
aws_vpc_id  = ""
aws_cidr_block_addr  = ""
# ssh key pair name in AWS to access instances
ssh_key_pair_name = "A2HA"
setup_managed_services = false
managed_elasticsearch_domain_url = ""
managed_elasticsearch_username = ""
managed_elasticsearch_user_password = ""
managed_elasticsearch_certificate = ""
managed_rds_instance_url = ""
managed_rds_superuser_username = ""
managed_rds_superuser_password = ""
managed_rds_dbuser_username = ""
managed_rds_dbuser_password = ""
managed_rds_certificate = ""
ami_filter_name = ""
ami_filter_virt_type = ""
ami_filter_owner = ""
ami_id = ""
# Enabale/Disable load balancer logs
lb_access_logs = "false"
automate_server_instance_type = "t3.medium"
chef_server_instance_type = "t3.medium"
elasticsearch_server_instance_type = "m5.large"
postgresql_server_instance_type = "t3.medium"
automate_lb_certificate_arn = "arn:aws:acm...."
chef_server_lb_certificate_arn = "arn:aws:acm...."
automate_ebs_volume_iops = "100"
automate_ebs_volume_size = "50"
automate_ebs_volume_type = "gp3"
chef_ebs_volume_iops = "100"
chef_ebs_volume_size = "50"
chef_ebs_volume_type = "gp3"
elasticsearch_ebs_volume_iops = "100"
elasticsearch_ebs_volume_size = "50"
elasticsearch_ebs_volume_type = "gp3"
postgresql_ebs_volume_iops = "100"
postgresql_ebs_volume_size = "50"
postgresql_ebs_volume_type = "gp3"
X-Contact = ""
X-Dept = ""
X-Project = ""
`

const haExistingNodesConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.existing_infra]
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
chef_lb_fqdn = ""

[automate.config]
# admin_password = ""
# automate load balancer fqdn IP or path
fqdn = ""
instance_count = "1"
# teams_port = ""
config_file = "configs/automate.toml"

[chef_server.config]
instance_count = "1"

[elasticsearch.config]
instance_count = "3"

[postgresql.config]
instance_count = "3"

[existing_infra.config]
automate_private_ips = []
chef_server_private_ips = []
elasticsearch_ips = []
elasticsearch_private_ips = []
postgresql_private_ips = []
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
