+++
title = "AWS Cloud Deployment Procedure"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Cloud Deployment Procedure"
    parent = "automate/install"
    identifier = "automate/install/ha_aws_deploy_steps.md AWS Cloud Deployment Procedure"
    weight = 350
+++

Follow the steps below to deploy Chef Automate HA on AWS (Amazon Web Services) cloud.

1. Open **Command Prompt**.
1. Login as a **root** user by typing `sudo su -`.
1. Type the command, `./chef-automate init-config-ha aws` and press **Enter** to set up the configuration for deployment. The `config.toml` configuration file generates with default settings and installs the latest deployment package.

<!-- Habitat is a package manager for the chef. A centralised place for all packages. -->

{{< figure src="/images/automate/ha_chef_automate_configtoml.png" alt="Chef Automate HA `config.toml` file">}}

1. Type the command, `cat config.toml` and press **Enter** to view the generated configuration file.

{{< figure src="/images/automate/ha_chef_automate_configtomldefault.png" alt="View Chef Automate HA Default Configuration">}}

1. Type the command, `./chef-automate provision-infra config.toml` and press **Enter**. This command downloads Habitat, creates deployment workspace (*/hab/a2_deploy_workspace*), and provisions AWS infrastructure.

1. Open the `config.toml` file in any editor and do the following:

1. Make the following changes in `config.toml` file by opening the file in an editor. For example, `vi config.toml`.

   - Specify the `ssh username` and the `ssh key file path`. The ssh key must resides in the bastion host where the Chef Automate deployment is taking place.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have the same value and the one used to provision the bastion EC2 instance.
   - Assign permission to the *ssh key file* by running command, `chmod 400 /root/.ssh/id_rsa`.
   - Specify the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes the value `1`.
   - Ensure not to modify the cluster number value as `1` for PostgreSQL and ElasticSearch.
   - Ensure the instance type supports the respective AWS region, volume type, size, and iops.
   <!-- 1. List of IP address for the cluster - there are options for private and public ip's. in case of we don't have public-ip for the vm's we can use the private ip -->
   <!-- Add load balancer certificate details for automate and chef-server. Navigate to Create Load Balancer screen in AWS console and copy the required LB ARN and DNS details-->

  {{< figure src="/images/automate/ha_aws_lb.png" alt="Load Balancer Details">}}
  {{< figure src="/images/automate/ha_aws_lb1.png" alt="Load Balancer and DNS Details in AWS">}}

   - Setup the secret management key and the required passwords. The default location for the secrets key and secret storage is set in the *config.toml* file. The default location for the key is `/etc/chef-automate/secrets.key` and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.
   - Specify *VPCID* and *CIDR* block. You can use the default available VPC from the AWS VPC page.

{{< figure src="/images/automate/ha_chef_automate_config_changes.png" alt="View Chef Automate Configuration File Edits">}}

1. Type the command, `./chef-automate deploy` and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

{{< figure src="/images/automate/ha_chef_automate_awsdeploy_complete.png" alt="Chef Automate AWS Deployment Confirmation">}}

1. Type the command, `cd /hab/a2_deploy_workspace` and press **Enter**. This command sets up the initial workspace directory and changes the working directory to the Chef Automate workspace configured.

1. Type the command, `./chef-automate status` and press **Enter**. This command displays the status of all nodes.

{{< figure src="/images/automate/ha_chef_automate_awsstatus.png" alt="Status on Chef Automate AWS Deploy Nodes">}}

1. Type the command, `./chef-automate info` and press **Enter**. This command displays all servers' IP addresses URL details of Kibana and Chef Automate.

{{< figure src="/images/automate/ha_chef_automate_awsinfo.png" alt="Detailed Information on Chef Automate AWS Deployment Infrastructure">}}
