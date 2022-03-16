+++
title = "AWS Deployment"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_deploy_aws.md AWS Deployment"
    weight = 50
+++

<!-- !-- Chef gonna give storage calculator for customer to provide req and derive their infrastructure.. this calc will be loaded into the doc page?? -->

This page explains how to deploy Chef Automate High Availability (HA) in your network premises/ infrastructure using Amazon Web Services (AWS). To deploy Chef Automate HA, execute the following steps in the listed order:

1. Set up the [Prerequisites for Chef Automate HA Deployment](( {{< relref "ha_architecture_reference.md#System Requirements" >}} )).
1. Obtain an AWS account or if you already have one, sign on to your AWS account.
1. Setup the [Bastion Host AWS requirements](( {{< relref "ha_bastion.md#Bastion Host Requirements for AWS (Amazon Web Services)" >}} )).
1. Ensure you have [Chef Automate utility](( {{< relref "ha_bastion.md#Download and Install the Chef Automate Utility" >}})) installed, else download and install the latest version.
1. Configure [Bastion Host for AWS ](( {{< relref "ha_bastion.md#Configuring Bastion for AWS Deployment Type" >}} )).
1. Connect your [Bastion host to your AWS instance](( {{< relref "#Establishing SSH Connection with Bastion Host" >}} )).
1. Create an [AWS Identity and Access Management IAM user](( {{< relref "ha_common.md#IAM Users.md" >}} )).
1. Create the certificate for the Chef Automate and Chef Server load balancers.
1. Create the certificates for security and authentication purposes. *optional*
1. Rotate the certificates if the certificates are expired or compromised. *optional*
1. Enable *dnshostname* in VPC, which determines whether the VPC supports assigning public DNS hostnames to instances with public IP addresses.
1. Execute [AWS Deployment commands](( {{< relref "#AWS Cloud Deployment Procedure" >}} )) to provision the Chef Automate HA on your cloud  network infrastructure.

{{< note >}}

A DNS hostname uniquely names a computer and consists of a host name and a domain name. DNS servers resolve DNS hostnames to their
corresponding IP addresses. To set up DNS in your VPC, ensure that DNS hostnames and DNS resolution are both enabled in your VPC.

Refer [Setting up DNS in Your VPC](https://docs.aws.amazon.com/glue/latest/dg/set-up-vpc-dns.html) page.

If the DNS attributes, *enableDnsSupport* and *enableDnsHostnames* are true, instances in the VPC is set with public DNS hostnames.
The default for these attributes are `false` when the VPC is a default VPC or the VPC is created using the VPC console wizard.

{{ < /note >}}

1. Execute the [Chef Automate HA deployment commands](( {{< relref "#AWS Cloud Deployment Procedured" >}} )).

## Establishing SSH Connection with Bastion Host

This section explains the procedure to establish the connection between your bastion host and an AWS account. A key pair consists of a public key that AWS stores and a private key file you store. Together, they allow you to connect to your instance securely.

1. Navigate to the AWS Management Console.
1. Select **Instances** > **EC2** option from the left menu.
1. Search your instance and select the corresponding Instance ID. The **AWS Console** displays the **Instance Summary** screen.
1. Select **Connect**. The AWS console displays various methods to connect to your instance. Here, we have used the Mac system.
1. Open an **SSH client**.
1. Locate your private key file and navigate to that directory.
1. Run the `chmod 400 key.pem` command (`key.pem` is the name of the key pair file name) to ensure your key is not publicly viewable.

![AWS EC2 Launch Status](/images/automate/ha_aws_connect.png)

{{< figure src="/images/automate/ha-aws-launch-status.png" alt="AWS EC2 Launch Status">}}

1. Connect to your instance using its public DNS. For example, `ssh -i "doc-bastion.pem" ubuntu@ec2-3-24-212-25.ap-southeast-2.compute.amazonaws.com`.
1. Select `yes` when the terminal prompts you to connect.

{{< figure src="/images/automate/ha_aws_ssh_connection.png" alt="AWS SSH Connection Details">}}

This completes the SSH connection to the AWS EC2 instance.

By default, you can log on as a Ubuntu user. You can switch to *root* access using the `sudo` command.

Refer ![mounting the file system on the EC2 instance and testing](https://docs.aws.amazon.com/efs/latest/ug/wt1-test.html) for detailed information.

## AWS Cloud Deployment Procedure

Follow these steps to deploy Chef Automate High Availability (HA) on AWS (Amazon Web Services) cloud.

1. Open **Command Prompt**.
1. Log in as a *Root* user by typing `sudo su -`.
1. Enter the `./chef-automate init-config-ha aws` command to set up the configuration for deployment. The `config.toml` configuration file generates and installs the latest deployment package with default settings.

<!-- Chef Habitat is a package manager for the chef. A centralized place for all packages. -->

{{< figure src="/images/automate/ha_chef_automate_configtoml.png" alt="Chef Automate HA `config.toml` file">}}

1. Enter the `cat config.toml` command to view the generated configuration file.

{{< figure src="/images/automate/ha_chef_automate_configtomldefault.png" alt="View Chef Automate HA Default Configuration">}}

1. Enter the `./chef-automate provision-infra config.toml` command. This command downloads Chef Habitat, creates deployment workspace (*/hab/a2_deploy_workspace*), and provisions AWS infrastructure.

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

 <!-- 1. List of IP address for the cluster - there are options for private and public ip's. in case of we don't have public-ip for the vm's we can use the private ip -->
   <!-- Add load balancer certificate details for chef automate and chef-server. Navigate to Create Load Balancer screen in AWS console and copy the required LB ARN and DNS details-->

1. Enter the `./chef-automate deploy` command. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

{{< figure src="/images/automate/ha_chef_automate_awsdeploy_complete.png" alt="Chef Automate AWS Deployment Confirmation">}}

1. Enter the `cd /hab/a2_deploy_workspace` command. This command sets up the initial workspace directory and changes the working directory to the Chef Automate workspace configured.

1. Enter the `./chef-automate status` command. This command displays the status of all nodes.

{{< figure src="/images/automate/ha_chef_automate_awsstatus.png" alt="Status on Chef Automate AWS Deploy Nodes">}}

1. Enter the `./chef-automate info` command. This command displays all servers' IP addresses URL details of **Kibana and Chef Automate**.

{{< figure src="/images/automate/ha_chef_automate_awsinfo.png" alt="Detailed Information on Chef Automate AWS Deployment Infrastructure">}}

## AWS Infrastructure Resources

The Chef Automate HA deployment using AWS creates the following network resources:

- Three instances for ElasticSearch node.

- Three instances for PostgreSQL node.

- One instance for Chef Automate server. However, based on your requirements, we can add more instances for the Chef Automate server.

- One instance for Chef Infra Server each. However, based on your requirements, we can add more instances for the Chef Infra server.

{{< figure src="/images/automate/ha_aws_resources1.png" alt="Chef Automate HA ElasticSearch and PostgreSQL Instances">}}

- Two load balancers and two respective target groups. One each for the Chef Automate server and Chef Infra server.

{{< figure src="/images/automate/ha_aws_resources2.png" alt="Chef Automate HA Load Balancers and Target Groups">}}

- Elastic File System (EFS) for backup of all the instances.

{{< figure src="/images/automate/ha_aws_resources3.png" alt="Chef Automate HA EFS Backup">}}

- Three private subnets (with no internet access) and three public subnets (with internet access).

{{< figure src="/images/automate/ha_aws_resources4.png" alt="Chef Automate HA Public and Private Subnets">}}

- One route table.

{{< figure src="/images/automate/ha_aws_resources5.png" alt="Chef Automate HA Route Table">}}

- One Elastic IP address.

{{< figure src="/images/automate/ha_aws_resources6.png" alt="Chef Automate HA Elastic IP">}}

## Clear AWS Deployment Infrastructure

Follow these steps to delete the *Terraform* and *HA Deployment Infrastructure*:

1. Navigate to the Chef Automate workspace folder where we have deployed HA. For example, `cd /hab/a2_deploy_workspace/terraform`.

1. Type the command, `terraform destroy` and press **Enter**. This command removes the terraform deployed at your workspace and all the instances.

1. Type the command, `rm -rf /hab/a2_deploy_workspace` and press **Enter**. This command removes the workspace.
