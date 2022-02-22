+++
title = "High Availability Provisioning Using AWS"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "High Availability Provisioning Using AWS"
    parent = "automate/install"
    identifier = "automate/install/ha_deploy_aws.md High Availability Provisioning Using AWS"
    weight = 270
+++

<!-- !-- Chef gonna give storage calculator for customer to provide req and derive their infrastructure.. this calc will be loaded into the doc page?? -->

This page explains how to deploy Chef Automate High Availability (HA) in your network premises/ infrastructure using Amazon Web Services (AWS). To deploy Chef Automate HA, execute the following steps in the listed order:

1. Set up the [Prerequisites for Chef Automate HA Deployment](( {{< relref "ha_system_requirements.md" >}} )).
1. Obtain an AWS account or if you already have one, sign on to your AWS account.
1. Setup the [Bastion Host AWS requirements](( {{< relref "ha_bastion.md#Bastion Host Requirements for AWS (Amazon Web Services)" >}} )).
1. Configure [Bastion Host for AWS ](( {{< relref "ha_aws_bastion.md#Bastion Host Configuration" >}} )).
1. Ensure you have [Chef Automate utility](( {{< relref "ha_auto_install.md" >}})) installed, else download and install the latest version.
1. Connect your [Bastion host to your AWS instance](( {{< relref "ha_aws_ssh_connect_bastion.md" >}} )).
1. Create an [AWS Identity and Access Management IAM user](( {{< relref "ha_iam_user.md" >}} )).
1. Create the certificate for the Chef Automate and Chef Server load balancers.
1. Create the certificates for security and authentication purposes. _optional_
1. Rotate the certificates if the certificates are expired or compromised. _optional_
1. Enable *dnshostname* in VPC, which determines whether the VPC supports assigning public DNS hostnames to instances with public IP addresses.
1. Execute [AWS Deployment commands](( {{< relref "ha_aws_deploy_steps.md" >}} )) to provision the Chef Automate HA on your cloud  network infrastructure.

{{< note >}}

A DNS hostname uniquely names a computer and consists of a host name and a domain name. DNS servers resolve DNS hostnames to their
corresponding IP addresses. To set up DNS in your VPC, ensure that DNS hostnames and DNS resolution are both enabled in your VPC.

Refer [Setting up DNS in Your VPC](https://docs.aws.amazon.com/glue/latest/dg/set-up-vpc-dns.html) page.

If the DNS attributes, *enableDnsSupport* and *enableDnsHostnames* are true, instances in the VPC is set with public DNS hostnames.
The default for these attributes are `false` when the VPC is a default VPC or the VPC is created using the VPC console wizard.

{{ < /note >}}

1. Execute the [Chef Automate HA deployment commands](( {{< relref "ha_aws_deploy_steps.md" >}} )).

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
