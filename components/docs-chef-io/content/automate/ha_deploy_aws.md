+++
title = "Chef Automate HA AWS Deployment Procedure"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Automate HA AWS Deployment Procedure"
    parent = "automate/install"
    identifier = "automate/install/ha_deploy_aws.md Chef Automate HA AWS Deployment Procedure"
    weight = 220
+++

!-- Chef gonna give storage calculator for customer to provide req and derive their infrastructure.. this calc will be loaded into the doc page??

This section explains the Amazon Web Services (AWS) deployment type to support Chef Automate HA in your network premises/ infrastructure.

## Pre-requisites

### Download and Install Chef Automate Utility

Both types of deployment models require you to install and configure Chef Automate on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.
- Type the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**. The command downloads the Chef Automate utility installer in .zip format.
- Type the  command,  and press **Enter**. The command installs the utility and provides the execute permission to the Chef Automate file.

  The installation of the Chef Automate utility completes and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}

### Building an AWS bastion host

Follow these steps to add a bastion host to your Linux environment on AWS cloud, which connects to other internal network hosts:

1. Sign into your AWS account. If you don't have one, sign up at https://aws.amazon.com.
1. Navigate to the AWS Management Console.

{{< figure src="/images/automate/ha-aws-console.png" alt="Amazon Management Console">}}

1. Select an AWS Region from the top toolbar.
1. Select EC2 under Services menu on the left.
1. Click the Launch Instances button, and perform the following steps:
   1. Select linux based Amazon Machine Image (AMI).

   {{< figure src="/images/automate/ha-aws-ami.png" alt="Amazon Machine Image">}}

   1. Select the *t2.medium* instance type. Ensure vCPUs is 1, Memory (GiB) is 4 and Instance Storage (GB) is EBS only.
   1. Click the Next: Configure Instance Details button.
   1. Modify VPC and Subnet values as required. 
   1. Ensure you have selected 1 in Number of instances field, and make any other required changes.
   1. Click Next: Add Storage button.
   1. Specify 100 GB of storage in Size (GiB) field.
   1. Click Next: Add Tags button.
   1. Specify the key and value for the tag in Key and Value fields. This step is optional.
   1. Click Next: Configure Security Group button.
   1. Select Create a new security group or Select an existing security group option.
   1. Based on your selection, select needed security groups for your EC2 instance, or add the rule by providing required details. You could change or update the security groups in the future if you want.
   1. Ensure Type is SSH, Protocol is TCP, and Port Range is 22 to create rules and connections.
   1. Open port 9631 by adding TCP rule.

Or, launch an EC2 instance, which was previously defined.

1. Click Review and Launch button.
1. Review all the details and click the Launch button. The AWS console prompts you to either create an existing SSH key pair, or use a pair you have previously established. 
   - If you choose to create a new key pair, specify a Key pair name and click Download Key Pair (private key file, .pem). Store the key file in a secure and accessible location.

    {{< figure src="/images/automate/ha-aws-keypair.png" alt="AWS EC2 Launch Instances">}}

   - Else, select an existing key pair. 
1. Click Launch Instances. The AWS console confirms the launch of your host.

 {{< figure src="/images/automate/ha-aws-launch-status.png" alt="AWS EC2 Launch Status">}}

You can test the connectivity to the bastion server by navigating to the AWS Console under Instances > EC2 option and view the fresh bastion server running.

For detailed information to deploy Linux bastion hosts to manage your AWS Cloud deployments remotely, see https://aws.amazon.com/quickstart/architecture/linux-bastion/.

### Connect to your Instance

A key pair consists of a public key that AWS stores, and a private key file that you store. Together, they allow you to connect to your instance securely.

1. Navigate to the AWS Management Console.
1. Select Instances > EC2 option from the left menu.
1. Search your instance and click the corresponding Instance ID. The AWS Console displays the Instance Summary screen.
1. Click Connect. The AWS console displays various methods to connect to your instance. Here, we have used Mac system.
1. Open an SSH client.
1. Locate your private key file and navigate to that directory. 
1. Run this command, `chmod 400 key.pem` (key.pem is the name of the key pair file name), to ensure your key is not publicly viewable.

![AWS EC2 Launch Status](/images/automate/ha-aws-connect.png)

{{< figure src="/images/automate/ha-aws-launch-status.png" alt="AWS EC2 Launch Status">}}

1. Connect to your instance using its public DNS. For example, `ssh -i "doc-bastion.pem" ubuntu@ec2-3-24-212-25.ap-southeast-2.compute.amazonaws.com`.
1. Type `yes` when terminal prompts with you for connecting.

{{< figure src="/images/automate/ha-aws-ssh-connection.png" alt="AWS SSH Connection Details">}}

This completes the SSH connection to the AWS EC2 instance.

## Deployment Procedure on AWS Cloud

Follow the steps below to deploy Chef Automate HA on AWS (Amazon Web Services) cloud.

1. Open **Command Prompt**.
1. Login as a **root** user by typing `sudo su -`.
1. Type the command, `./chef-automate init-config-ha aws` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings and installs latest deployment package.

{{< figure src="/images/automate/ha_chef_automate_configtoml.png" alt="Chef Automate HA `config.toml` file">}}

1. Type the command, `cat config.toml` and press **Enter** to view the generated configuration file.

{{< figure src="/images/automate/ha_chef_automate_configtomldefault.png" alt="View Chef Automate HA Default Configuration">}}

1. Type the command, `./chef-automate provision-infra config.toml` and press **Enter**. This command downloads Habitat, creates deployment workspace (*/hab/a2_deploy_workspace*), and provisions the infrastructure on AWS.

1. Open the `config.toml` file in any editor and do the following:

1. Make the following changes in `config.toml` file by opening the file in a editor. For example, `vi config.toml`.

   1. Specify the `ssh username` and the `ssh key file path`. The ssh key must reside in bastion host.
   1. Ensure `ssh_key_pair_name` and `ssh key file path` have same value.
   1. Assign permission to the *ssh key file* by running command, `chmod 400 /root/.ssh/id_rsa`.
   1. Specify the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes value `1`. 
   1. Ensure not to modify the cluster number value as `1` for PostgreSQL and ElasticSearch.
   1. Ensure the instance type supports the respective AWS region, volume type, size and iops.
   1. List of IP address for the cluster - there are options for private and public ip's. in case of we don't have public-ip for the vm's we can use the private ip
   1. Setup the secrets management key and the required passwords. The default location for the secrets key and secret storage is set in the *config.toml* file. The default location for the key is `/etc/chef-automate/secrets.key` and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.
   1. Specify VPCID and CIDR block. You can use the default available VPC from AWS VPC page.

{{< figure src="/images/automate/ha_chef_automate_config_changes.png" alt="View Chef Automate Configuration File Edits">}}

1. Type the command, `./chef-automate deploy` and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure.

{{< figure src="/images/automate/ha_chef_automate_awsdeploy_complete.png" alt="Chef Automate AWS Deployment Confirmation">}}

1. Type the command, `cd /hab/a2_deploy_workspace` and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

1. Type the command, `./chef-automate status` and press **Enter**. This command displays the status of all nodes.

{{< figure src="/images/automate/ha_chef_automate_awsstatus.png" alt="Status on Chef Automate AWS Deploy Nodes">}}

1. Type the command, `./chef-automate info` and press **Enter**. This command displays all server's IP addressses URL details of Kibana and Chef Automate.

{{< figure src="/images/automate/ha_chef_automate_awsinfo.png" alt="Detailed Information on Chef Automate AWS Deployment Infrastructure">}}

### AWS Infrastructure Resources

The Chef Automate HA deployment using AWS creates the following network resources:

- Three instances for ElasticSearch node.

- Three instances for PostgreSQL node.

- One instance for Chef Automate server. However, based on your requirements we can add more instances for Chef Automate server.

- One instance for Chef Infra Server each. However, based on your requirements we can add more instances for Chef Infra server.

{{< figure src="/images/automate/ha-aws-resources1.png" alt="Chef Automate HA ElasticSearch and PostgreSQL Instances">}}

- Two load balancers and two respective target groups. One each for Chef Automate server and Chef Infra server.

{{< figure src="/images/automate/ha-aws-resources2.png" alt="Chef Automate HA Load Balancers and Target Groups">}}

- Elastic File System (EFS) for backup of all the instances.

{{< figure src="/images/automate/ha-aws-resources3.png" alt="Chef Automate HA EFS Backup">}}

- Three private subnets (with no internet access) and three public subnets (with internet access).

{{< figure src="/images/automate/ha-aws-resources4.png" alt="Chef Automate HA Public and Private Subnets">}}

- One route table.

{{< figure src="/images/automate/ha-aws-resources5.png" alt="Chef Automate HA Route Table">}}

- One Elastic IP address.

{{< figure src="/images/automate/ha-aws-resources6.png" alt="Chef Automate HA Elastic IP">}}

## Clear AWS Deployment Infrastructure

Follow these steps to delete the *Terraform* and *HA Deployment Infrastructure*:

1. Navigate to the Chef Automate workspace folder where we have deployed HA. For example, `cd /hab/a2_deploy_workspace/terraform`.

1. Type the command, `terraform destroy` and press **Enter**. This command removes the terraform deployed at your workspace.

1. Type the command, `rm -rf /hab/a2_deploy_workspace` and press **Enter**.
