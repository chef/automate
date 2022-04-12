+++
title = "Deployment Procedure"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Deployment Procedure"
    parent = "automate/deploy_high_availability/aws_deployment"
    identifier = "automate/deploy_high_availability/aws_deployment/ha_aws_deploy_steps.md Deployment Procedure"
    weight = 230
+++

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

1. Open **Command Prompt**.
2. Log in as a *Root* user by typing `sudo su -`.
3. Execute the `./chef-automate init-config-ha aws` command to set up the configuration for deployment. The `config.toml` configuration file generates and installs the latest deployment package with default settings.

<!-- Chef Habitat is a package manager for the chef. A centralized place for all packages. -->

{{< figure src="/images/automate/ha_chef_automate_configtoml.png" alt="Chef Automate HA `config.toml` file">}}

4. Execute the `cat config.toml` command to view the generated configuration file.

{{< figure src="/images/automate/ha_chef_automate_configtomldefault.png" alt="View Chef Automate HA Default Configuration">}}

5. Execute the `./chef-automate provision-infra config.toml` command. This command downloads Chef Habitat creates deployment workspace (*/hab/a2_deploy_workspace*), and provisions AWS infrastructure.

By default, the configuration file has the following contents:

```ruby
# This is a Chef Automate AWS HA mode configuration file. You can run
# 'chef-automate deploy' with this config file, and it should
# successfully create a new Chef Automate HA instance with default settings.

[architecture.aws]
secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
architecture = "aws"
workspace_path = "/hab/a2_deploy_workspace"

# ssh user name for ssh login to instance like default user for centos will centos or for red-hat will be ec2-user
ssh_user = "centos"
# private ssh key file path to access instances
ssh_key_file = "/home/ubuntu/a2ha-london.pem"
# sudo_password = ""
# logging_monitoring_management = ""
# new_elk = ""
# existing_elk_instance_ip ""
# existing_elk_port ""
# existing_elk_cert ""
# existing_elk_username ""
# existing_elk_password ""
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
region = "eu-west-2"
# Provide vpcid and cidr block
# E.g. aws_vpc_id = "vpc12318h"
# E.g. aws_cidr_block_addr = "172.31.64.0"
aws_vpc_id  = "vpc-24f8cc4d"
aws_cidr_block_addr  = "172.31.128.0"
# ssh key pair name in AWS to access instances
ssh_key_pair_name = "a2ha-london"
ami_filter_name = ""
ami_filter_virt_type = ""
ami_filter_owner = ""
ami_id = ""
automate_server_instance_type = "t3.medium"
chef_server_instance_type = "t3.medium"
elasticsearch_server_instance_type = "m5.large"
postgresql_server_instance_type = "t3.medium"
automate_lb_certificate_arn = "arn:aws:acm:eu-west-2:112758395563:certificate/508ef207-0f30-4fd4-9c5b-dc76f40915f1"
chef_server_lb_certificate_arn = "arn:aws:acm:eu-west-2:112758395563:certificate/508ef207-0f30-4fd4-9c5b-dc76f40915f1"
automate_ebs_volume_iops = "100"
automate_ebs_volume_size = "50"
automate_ebs_volume_type = "gp2"
chef_ebs_volume_iops = "100"
chef_ebs_volume_size = "50"
chef_ebs_volume_type = "gp2"
elasticsearch_ebs_volume_iops = "100"
elasticsearch_ebs_volume_size = "50"
elasticsearch_ebs_volume_type = "gp2"
postgresql_ebs_volume_iops = "100"
postgresql_ebs_volume_size = "50"
postgresql_ebs_volume_type = "gp2"
X-Contact = "ffulara@progress.com"
X-Dept = ""
X-Project = ""
```

6. Make the following changes in `config.toml` file by opening the file in an editor:

   - Specify the `ssh username` and the `ssh key file path`. The SSH user and SSH key must be in the bastion host where the Chef Automate deployment occurs.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have the same value as the one used to provision the bastion EC2 instance.
   - Assign permission to the **ssh key file** by executing command, `chmod 400 /root/.ssh/id_rsa`.
   - Enter the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes the value `1`.
   - Ensure the `architecture` field has value as `aws` and the region in which you want to deploy is specified in the region field.
   - Ensure not to change the cluster number value as `1` for PostgreSQL and ElasticSearch.
   - Ensure the instance type supports the respective AWS region, volume type, size, and iops.

    {{< figure src="/images/automate/ha_aws_lb.png" alt="Load Balancer Details">}}

    {{< figure src="/images/automate/ha_aws_lb1.png" alt="Load Balancer and DNS Details in AWS">}}

   - Setup the secret management key and the required passwords. The default location for the secrets key and secret storage is defined in the *config.toml* file. The default location for the key is `/etc/chef-automate/secrets.key`, and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

     {{< figure src="/images/automate/ha_aws_secretkeys.png" alt="AWS Access Key & Secret Access Key">}}

   - Specify the ARN (Amazon Resource Name) of the certificate used to create a load balancer.

   - Specify *VPCID* and *CIDR* block. You can also use the default available VPC from the AWS VPC page.

{{< figure src="/images/automate/ha_chef_automate_config_changes.png" alt="View Chef Automate Configuration File Edits">}}

 <!-- 1. List of IP addresses for the cluster - options for private and public IP's. in case we don't have public-ip for the vm's we can use the private ip -->
   <!-- Add load balancer certificate details for chef automate and chef-server. Navigate to Create Load Balancer screen in AWS console and copy the required LB ARN and DNS details-->

7. Execute the `./chef-automate deploy` command. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

{{< figure src="/images/automate/ha_chef_automate_awsdeploy_complete.png" alt="Chef Automate AWS Deployment Confirmation">}}

8. Execute the `cd /hab/a2_deploy_workspace` command. This command sets up the initial workspace directory and changes the working directory to the Chef Automate workspace configured.

9. Execute the `./chef-automate status` command. This command displays the quality of all nodes.

{{< figure src="/images/automate/ha_chef_automate_awsstatus.png" alt="Status on Chef Automate AWS Deploy Nodes">}}

10. Execute the `./chef-automate info` command. This command displays all servers' IP addresses and URL details of **Kibana and Chef Automate**.

{{< figure src="/images/automate/ha_chef_automate_awsinfo.png" alt="Detailed Information on Chef Automate AWS Deployment Infrastructure">}}
