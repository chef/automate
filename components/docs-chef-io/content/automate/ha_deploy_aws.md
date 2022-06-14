+++
title = "AWS Deployment Model"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment Model"
    parent = "automate/deploy_high_availability/aws_deployment"
    identifier = "automate/deploy_high_availability/aws_deployment/ha_deploy_aws.md AWS Deployment Model"
    weight = 210
+++

<!-- !-- Chef going to give storage calculator for a customer to provide req and derive their infrastructure.. this calc will be loaded into the doc page?? -->

This page explains how to deploy Chef Automate High Availability (HA) in your network premises/ infrastructure using Amazon Web Services (AWS).

<<<<<<< HEAD
## Prerequisite
=======
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

{{< /note >}}

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
>>>>>>> main

Based on the number of nodes, the virtual machine requirements for AWS deployment are as follows:

| Instance          | Type         | RAM                                                   | Volume Size         | Volume Type | Volume iops |
| :---------------  | :----------  | :---------------------------------------------------  | :-----------------  | :---------  | :---------  |
| PostgreSQL        | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 150 |
| Opensearch     | m5.large     | 8 GB RAM for test and 16 GB for production. vCPU - 2. | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 300 |
| Chef Automate     | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 100 |
| Chef Infra Server | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 100 |

{{< note >}}

ES volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans. The above table includes instances’ RAM and volume size, set up for testing purposes. Production depends on the number of nodes and the frequency of Chef Infra Client runs and compliance scans. However, for on-premises deployment, you can choose the above requirements for VM like RAM.

For **Opensearch** and **PostgresSQL**, a minimum of three node clusters is required.

{{< /note >}}

## Deploy Procedure

To deploy Chef Automate HA, execute the following steps in the listed order:

1. Set up the [Prerequisites for Chef Automate HA Deployment]({{< relref "ha_platform_support.md" >}}).
1. Obtain an AWS account or if you already have one, sign on to your AWS account.
1. Setup the [Bastion Host AWS requirements]({{< relref "ha_bastion.md#Bastion Host Requirements for AWS (Amazon Web Services)" >}}).
1. Configure [Bastion Host for AWS]({{< relref "ha_aws_bastion.md#Bastion Host Configuration" >}}).
1. Ensure you have [Chef Automate utility]({{< relref "ha_auto_install.md" >}}) installed, else download and install the latest version.
1. Connect your [Bastion host to your AWS instance]({{< relref "ha_aws_ssh_connect_bastion.md" >}}).
1. Create an [AWS Identity and Access Management IAM user]({{< relref "ha_iam_user.md" >}}).
1. Create the certificate for the Chef Automate and Chef Server load balancers.
1. [Create the certificates]({{< relref "ha_cert_selfsign.md" >}}) for security and authentication purposes. _optional_
1. [Rotate the certificates]({{< relref "ha_cert_rotaion.md" >}}) if the certificates are expired or compromised. _optional_
1. Enable **dnshostname** in VPC, which determines whether the VPC supports assigning public DNS hostnames to instances with public IP addresses.
1. Execute [AWS Deployment commands]({{< relref "ha_aws_deploy_steps.md" >}}) to provision the Chef Automate HA on your cloud network infrastructure.
1. Execute the [Chef Automate HA deployment commands]({{< relref "ha_aws_deploy_steps.md" >}}).

{{< note >}}

A DNS hostname uniquely names a computer and consists of a hostname and a domain name. DNS servers resolve DNS hostnames to their corresponding IP addresses. To set up DNS in your VPC, ensure that DNS hostnames and DNS resolution are enabled in your VPC.

Refer to [Setting up DNS in Your VPC](https://docs.aws.amazon.com/glue/latest/dg/set-up-vpc-dns.html) page.

If the DNS attributes **enableDnsSupport** and **enableDnsHostnames** are true, instances in the VPC are set with public DNS hostnames. The default for these attributes is `false` when the VPC is a default VPC or the VPC is created using the VPC console wizard.

{{< /note >}}

## AWS Infrastructure Resources

The Chef Automate HA deployment using AWS creates the following network resources:

- Three instances for OpenSearch node.

- Three instances for PostgreSQL node.

- One instance for Chef Automate server. However, we can add more instances for the Chef Automate server based on your requirements.

- One instance for Chef Infra Server each. However, we can add more instances for the Chef Infra server based on your requirements.

{{< figure src="/images/automate/ha_aws_resources1.png" alt="Chef Automate HA OpenSearch and PostgreSQL Instances">}}

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

Follow these steps to delete the **Terraform** and **HA Deployment Infrastructure**:

1. Navigate to the Chef Automate workspace folder where we have deployed HA. For example, `cd /hab/a2_deploy_workspace/terraform`.

1. Execute the command `terraform destroy`. This command removes the terraform deployed at your workspace and all the instances.

1. Execute the command `rm -rf /hab/a2_deploy_workspace`. This command removes the workspace.
