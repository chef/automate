package main

const haAwsConfigTemplate = `
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file and it should
# successfully create a new Chef Automate HA instances with default settings.

[architecture.aws]
## === INPUT NEEDED ===
# Eg.: ssh_user = "ubuntu"
ssh_user = ""

# custome ssh port no to connect instances, default will be 22
# Eg.: ssh_port = "22"
ssh_port = ""

# Private SSH key file path, which has access to all the instances.
# Eg.: ssh_key_file = "~/.ssh/A2HA.pem"
ssh_key_file = ""

# Provide Password if needed to run sudo commands.
# sudo_password = ""

# Eg.: backup_config = "efs" or "s3"
backup_config = ""

# If s3 is selected for backup_config,
#    then uncomment and give s3_bucketName 
#    or else default chef-automate-ha.<deployment-string> will be used.
# s3_bucketName = ""
## === ===

secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "aws"
workspace_path = "/hab/a2_deploy_workspace"

# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"


[automate.config]
## === INPUT NEEDED ===

# Password for Automate UI for 'admin' user.
admin_password = ""

# Automate Load Balancer FQDN eg.: "chefautomate.example.com"
fqdn = ""

# No. of Automate Frontend Machine or VM eg.: instance_count = "2"
instance_count = ""

## === ===

#Deprecated Config - automate_setup_type is not supported
# automate_setup_type = "automate"

# teams_port = ""
config_file = "configs/automate.toml"

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Automate load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""


[chef_server.config]
## === INPUT NEEDED ===

# No. of Chef Server Frontend Machine or VM eg.: instance_count = "2"
instance_count = ""

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Chef Server load balancer root-ca and keys
# private_key = ""
# public_key = ""

## === ===


[opensearch.config]
## === INPUT NEEDED ===

# No. of OpenSearch DB Backend Machine or VM eg.: instance_count = "3"
instance_count = ""

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add OpenSearch load balancer root-ca and keys
# root_ca = ""
# admin_key = ""
# admin_cert = ""
# private_key = ""
# public_key = ""

## === ===

[postgresql.config]
## === INPUT NEEDED ===

# No. of Postgresql DB Backend Machine or VM eg.: instance_count = "3"
instance_count = ""

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Postgresql load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""

## === ===


# ======================================================

[aws.config]
# ============== AWS network Config ============================
## === INPUT NEEDED ===

# Eg.: profile = "default"
profile = ""

# Eg.: region = "us-east-1"
region = ""

# Provide vpcid and cidr block
# Eg.: aws_vpc_id = "vpc12318h"
aws_vpc_id  = ""

# Eg.: aws_cidr_block_addr = "172.31.64.0"
aws_cidr_block_addr  = ""

# Eg.: private_custom_subnets = ["subnet-e556d512", "subnet-e556d513", "subnet-e556d514"]
private_custom_subnets = []

# Eg.: public_custom_subnets = ["subnet-p556d512", "subnet-p556d513", "subnet-p556d514"]
public_custom_subnets = []

# ssh key pair name in AWS to access instances
# eg: ssh_key_pair_name = "A2HA"
ssh_key_pair_name = ""

## === ===

# ============== Managed Services ======================

## === INPUT NEEDED ===
# in case your are trying to deploy with aws managed 
# RDS, and openseach, then make setup_managed_services = true, 
# and modify other managed services settings.
setup_managed_services = false

# eg: managed_opensearch_domain_name = "managed-services-os"
managed_opensearch_domain_name = ""

# eg: managed_opensearch_domain_url = "search-managed-services-os-eckom3msrwqlmjlgbdu.us-east-1.es.amazonaws.com"
managed_opensearch_domain_url = ""

# eg: managed_opensearch_username = "admin"
managed_opensearch_username = ""

# eg: managed_opensearch_user_password = "Progress@123"
managed_opensearch_user_password = ""

# eg: managed_opensearch_certificate = "<cert content>"
managed_opensearch_certificate = ""

# eg: aws_os_snapshot_role_arn = "arn:aws:iam::1127583934333:role/managed-services"
aws_os_snapshot_role_arn = ""

# eg: os_snapshot_user_access_key_id = "AKIA..........PQS7Q7A"
os_snapshot_user_access_key_id = ""

# eg: os_snapshot_user_access_key_secret = "skP4Mqihj....................anAXAX"
os_snapshot_user_access_key_secret = ""

# eg: managed_rds_instance_url = "managed-rds-db.cww4poze5gkx.ap-northeast-1.rds.amazonaws.com:5432"
managed_rds_instance_url = ""

# eg: managed_rds_superuser_username = "postgres"
managed_rds_superuser_username = ""

# eg: managed_rds_superuser_password = "Progress123"
managed_rds_superuser_password = ""

# eg: managed_rds_dbuser_username = "postgres"
managed_rds_dbuser_username = ""

# eg: managed_rds_dbuser_password = "Progress123"
managed_rds_dbuser_password = ""

# eg: managed_rds_certificate = "<cert content>"
managed_rds_certificate = ""

## === ===

# ======================================================

# ============== EC2 Instance Config ===================

## === INPUT NEEDED ===

# This AMI should be from the Same Region which we selected above.
# eg: ami_id = "ami-08d4ac5b634553e16" # This ami is of Ubuntu 20.04 in us-east-1
ami_id = ""

# eg: delete_on_termination = true or false
delete_on_termination = true

# eg: automate_server_instance_type = "t3.medium"
automate_server_instance_type = ""

# eg: chef_server_instance_type = "t3.medium"
chef_server_instance_type = ""

# eg: opensearch_server_instance_type = "m5.large"
opensearch_server_instance_type = ""

# eg: postgresql_server_instance_type = "t3.medium"
postgresql_server_instance_type = ""

# eg: automate_lb_certificate_arn = "arn:aws:acm...."
automate_lb_certificate_arn = ""

# eg: chef_server_lb_certificate_arn = "arn:aws:acm...."
chef_server_lb_certificate_arn = ""

# eg: automate_ebs_volume_iops = "100"
automate_ebs_volume_iops = ""

# eg: automate_root_ebs_volume_iops = "100"
automate_root_ebs_volume_iops = ""

# eg: automate_ebs_volume_size = "50"
automate_ebs_volume_size = ""

# eg: automate_root_ebs_volume_size = "50"
automate_root_ebs_volume_size = ""

# eg: automate_ebs_volume_type = "gp3"
automate_ebs_volume_type = ""

# eg: automate_root_ebs_volume_type = "gp3"
automate_root_ebs_volume_type = ""

# eg: chef_ebs_volume_iops = "100"
chef_ebs_volume_iops = ""

# eg: chef_root_ebs_volume_iops = "100"
chef_root_ebs_volume_iops = ""

# eg: chef_ebs_volume_size = "50"
chef_ebs_volume_size = ""

# eg: chef_root_ebs_volume_size = "50"
chef_root_ebs_volume_size = ""

# eg: chef_ebs_volume_type = "gp3"
chef_ebs_volume_type = ""

# eg: chef_root_ebs_volume_type = "gp3"
chef_root_ebs_volume_type = ""

# eg: opensearch_ebs_volume_iops = "100"
opensearch_ebs_volume_iops = ""

# eg: opensearch_root_ebs_volume_iops = "100"
opensearch_root_ebs_volume_iops = ""

# eg: opensearch_ebs_volume_size = "50"
opensearch_ebs_volume_size = ""

# eg: opensearch_root_ebs_volume_size = "50"
opensearch_root_ebs_volume_size = ""

# eg: opensearch_ebs_volume_type = "gp3"
opensearch_ebs_volume_type = ""

# eg: opensearch_root_ebs_volume_type = "gp3"
opensearch_root_ebs_volume_type = ""

# eg: postgresql_ebs_volume_iops = "100"
postgresql_ebs_volume_iops = ""

# eg: postgresql_root_ebs_volume_iops = "100"
postgresql_root_ebs_volume_iops = ""

# eg: postgresql_ebs_volume_size = "50"
postgresql_ebs_volume_size = ""

# eg: postgresql_root_ebs_volume_size = "50"
postgresql_root_ebs_volume_size = ""

# eg: postgresql_ebs_volume_type = "gp3"
postgresql_ebs_volume_type = ""

# eg: postgresql_root_ebs_volume_type = "gp3"
postgresql_root_ebs_volume_type = ""

## === ===

# Enabale/Disable load balancer logs
# eg lb_access_logs = "false"
lb_access_logs = "false"

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
## === INPUT NEEDED ===
# Eg.: ssh_user = "ubuntu"
ssh_user = ""

# private ssh key file path to access instances
# Eg.: ssh_user = "~/.ssh/A2HA.pem"
ssh_key_file = ""

# custome ssh port no to connect instances, default will be 22
# Eg.: ssh_port = "22"
ssh_port = ""

# Provide Password if needed to run sudo commands.
sudo_password = ""
## === ===

secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "existing_nodes"
workspace_path = "/hab/a2_deploy_workspace"
# DON'T MODIFY THE BELOW LINE (backup_mount)
backup_mount = "/mnt/automate_backups"

# Eg.: backup_config = "object_storage" or "file_system"
backup_config = ""

# If backup_config = "object_storage" fill out [object_storage.config] as well 
## Object storage similar to AWS S3 Bucket
[object_storage.config]
bucket_name = ""
access_key = ""
secret_key = ""
# For S3 bucket, default endpoint value is "https://s3.amazonaws.com"
# Include protocol to the enpoint value. Eg: https://customdns1.com or http://customdns2.com
endpoint = ""

# [Optional] Mention object_storage region if applicable
# Eg: region = "us-west-1"
region = ""
## === ===

# ============== EC2 Nodes Config ======================
[automate.config]
## === INPUT NEEDED ===

# Password for Automate UI for 'admin' user.
# admin_password = ""


# Automate Load Balancer FQDN eg.: "chefautomate.example.com"
fqdn = ""

# No. of Automate Frontend Machine or VM eg.: instance_count = "2"
instance_count = ""

## === ===


# teams_port = ""
config_file = "configs/automate.toml"

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Automate load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""
# [[automate.config.certs_by_ip]]
# ip = ""
# private_key = ""
# public_key = ""

[chef_server.config]
## === INPUT NEEDED ===

# No. of Chef Server Frontend Machine or VM eg.: instance_count = "2"
instance_count = ""

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add Chef Server load balancer root-ca and keys
# private_key = ""
# public_key = ""
# [[chef_server.config.certs_by_ip]]
# ip = ""
# private_key = ""
# public_key = ""

## === ===

[opensearch.config]
## === INPUT NEEDED ===

# No. of OpenSearch DB Backend Machine or VM eg.: instance_count = "3"
instance_count = ""

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add OpenSearch load balancer root-ca and keys
# root_ca = ""
# admin_key = ""
# admin_cert = ""
# private_key = ""
# public_key = ""
# [[opensearch.config.certs_by_ip]]
# ip = ""
# private_key = ""
# public_key = ""

## === ===

[postgresql.config]
## === INPUT NEEDED ===

# No. of Postgresql DB Backend Machine or VM eg.: instance_count = "3"
instance_count = ""

# Set enable_custom_certs = true to provide custom certificates during deployment
enable_custom_certs = false
# Add postgresql load balancer root-ca and keys
# root_ca = ""
# private_key = ""
# public_key = ""
# [[postgresql.config.certs_by_ip]]
# ip = ""
# private_key = ""
# public_key = ""

## === ===

[existing_infra.config]
## === INPUT NEEDED ===

# provide comma-seperated IP addresses of nodes, like ["192.0.0.1", "192.0.0.2", "192.0.0.2"]
# No. of IP address should be the same as the No. of instance_count count mentioned above in
# automate.config, chef_server.config, opensearch.config and postgresql.config
automate_private_ips = []
chef_server_private_ips = []
opensearch_private_ips = []
postgresql_private_ips = []
## === ===

# ============== External Datbase Services ======================

## === INPUT NEEDED ===
# In case you are trying to deploy with AWS Managed Services set type as "aws"
# If you are trying external managed database set type as "self-managed"

[external.database]

# eg type = "aws" or "self-managed"
type = ""

[external.database.postgre_sql]

# eg: instance_url = "managed-rds-db.c5gkx.ap-northeast-1.rds.amazonaws.com:5432"
instance_url = ""

# eg: username = "postgres"
superuser_username = ""

# eg: password = "Progress123"
superuser_password = ""

# eg: dbuser_username = "postgres"
dbuser_username = ""

# eg: dbuser_password = "Progress123"
dbuser_password = ""

# In case of AWS managed RDS leave it blank
postgresql_root_cert = "<cert_content>"

[external.database.open_search]

# eg: managed_opensearch_domain_name = "managed-services-os"
opensearch_domain_name = ""

# eg: opensearch_domain_url = "search-managed-services-os.us-east-1.es.amazonaws.com"
opensearch_domain_url = ""

# eg: opensearch_username = "admin"
opensearch_username = ""

# eg: opensearch_user_password = "Progress@123"
opensearch_user_password = ""

# In case of AWS managed opensearch leave it blank
opensearch_root_cert = "<cert_content>"

[external.database.open_search.aws]

# eg: aws_os_snapshot_role_arn = "arn:aws:iam::1127583934333:role/managed-services"
aws_os_snapshot_role_arn = ""

# eg: os_snapshot_user_access_key_id = "AKIA..........PQS7Q7A"
os_snapshot_user_access_key_id = ""

# eg: os_snapshot_user_access_key_secret = "skP4Mqihj....................anAXAX"
os_snapshot_user_access_key_secret = ""

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
