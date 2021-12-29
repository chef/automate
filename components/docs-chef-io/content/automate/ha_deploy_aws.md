+++
title = "Chef Automate HA AWS Deployment Procedure"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Automate HA AWS Deployment Procedure"
    parent = "automate/install"
    identifier = "automate/install/ha_deploy_aws.md Chef Automate HA AWS Deployment Procedure"
    weight = 210
+++

!-- Chef gonna give storage calculator for customer to provide req and derive their infrastructure.. this calc will be loaded into the doc page??

This page explains the procedure to deploy Chef Automate High Availability (HA) in your network premises/ infrastructure using Amazon Web Services (AWS). To deploy Chef Automate HA, execute the following steps in the listed order:

- Set up the [prerequisites for Chef Automate HA Deployment]({{< relref "ha_aws_prereq.md" >}}) section.
- Connect your [Bastion host to your AWS instance]({{< relref "ha_aws_connect_bastion.md" >}}) section.
- Create an [AWS Identity and Access Management (IAM) user]({{< relref "ha_iam_user.md" >}}) section.
- Configure the [AWS credentials on your Bastion host]({{< relref "ha_configure_aws_credentials.md" >}}) section.
- Create the DNS certificate.
- Rotate the AWS certificates.
- [Chef Automate HA deployment commands]({{< relref "ha_aws_deploy_steps.md" >}}) section.

## AWS Infrastructure Resources

The Chef Automate HA deployment using AWS creates the following network resources:

- Three instances for ElasticSearch node.

- Three instances for PostgreSQL node.

- One instance for Chef Automate server. However, based on your requirements we can add more instances for Chef Automate server.

- One instance for Chef Infra Server each. However, based on your requirements we can add more instances for Chef Infra server.

{{< figure src="/images/automate/ha_aws_resources1.png" alt="Chef Automate HA ElasticSearch and PostgreSQL Instances">}}

- Two load balancers and two respective target groups. One each for Chef Automate server and Chef Infra server.

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
