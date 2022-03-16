+++
title = "Deployment Procedure"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Deployment Procedure"
    parent = "automate/deploy_high_availability/aws_deployment"
    identifier = "automate/deploy_high_availability/aws_deployment/ha_aws_deploy_steps.md Deployment Procedure"
    weight = 260
+++

Follow the steps below to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

1. Open **Command Prompt**.
1. Log in as a *Root* user by typing `sudo su -`.
1. Execute the `./chef-automate init-config-ha aws` command to set up the configuration for deployment. The `config.toml` configuration file generates and installs the latest deployment package with default settings.

<!-- Chef Habitat is a package manager for the chef. A centralized place for all packages. -->

{{< figure src="/images/automate/ha_chef_automate_configtoml.png" alt="Chef Automate HA `config.toml` file">}}

1. Execute the `cat config.toml` command to view the generated configuration file.

{{< figure src="/images/automate/ha_chef_automate_configtomldefault.png" alt="View Chef Automate HA Default Configuration">}}

1. Execute the `./chef-automate provision-infra config.toml` command. This command downloads Chef Habitat, creates deployment workspace (*/hab/a2_deploy_workspace*), and provisions AWS infrastructure.

1. Make the following changes in `config.toml` file by opening the file in an editor:

   - Specify the `ssh username` and the `ssh key file path`. The ssh key must be in the bastion host where the Chef Automate deployment occurs.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have the same value and the one used to provision the bastion EC2 instance.
   - Assign permission to the **ssh key file** by executing command, `chmod 400 /root/.ssh/id_rsa`.
   - Enter the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes the value `1`.
   - Ensure not to change the cluster number value as `1` for PostgreSQL and ElasticSearch.
   - Ensure the instance type supports the respective AWS region, volume type, size, and iops.
  
    {{< figure src="/images/automate/ha_aws_lb.png" alt="Load Balancer Details">}}

    {{< figure src="/images/automate/ha_aws_lb1.png" alt="Load Balancer and DNS Details in AWS">}}

   - Setup the secret management key and the required passwords. The default location for the secrets key and secret storage is defined in the *config.toml* file. The default location for the key is `/etc/chef-automate/secrets.key`, and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

   - Specify *VPCID* and *CIDR* block. You can also use the default available VPC from the AWS VPC page.

{{< figure src="/images/automate/ha_chef_automate_config_changes.png" alt="View Chef Automate Configuration File Edits">}}

 <!-- 1. List of IP addresses for the cluster - there are options for private and public ip's. in case we don't have public-ip for the vm's we can use the private ip -->
   <!-- Add load balancer certificate details for chef automate and chef-server. Navigate to Create Load Balancer screen in AWS console and copy the required LB ARN and DNS details-->

1. Execute the `./chef-automate deploy` command. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

{{< figure src="/images/automate/ha_chef_automate_awsdeploy_complete.png" alt="Chef Automate AWS Deployment Confirmation">}}

1. Execute the `cd /hab/a2_deploy_workspace` command. This command sets up the initial workspace directory and changes the working directory to the Chef Automate workspace configured.

1. Execute the `./chef-automate status` command. This command displays the status of all nodes.

{{< figure src="/images/automate/ha_chef_automate_awsstatus.png" alt="Status on Chef Automate AWS Deploy Nodes">}}

1. Execute the `./chef-automate info` command. This command displays all servers' IP addresses URL details of **Kibana and Chef Automate**.

{{< figure src="/images/automate/ha_chef_automate_awsinfo.png" alt="Detailed Information on Chef Automate AWS Deployment Infrastructure">}}
