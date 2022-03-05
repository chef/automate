+++
title = "Deployment Workflows"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Deployment Workflows"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_workflow.md Deployment Workflows"
    weight = 30
+++

This page includes the two types of Chef Automate High Availability (HA) Workflows in words and infographic format.

## Bare Infra Deployment

1. Set the software and hardware requirements.
1. Obtain necessary virtual machine (VM) instance details (with private IP addresses and added public address for Elasticsearch) to create the cluster of the **Chef Automate** , **Chef Server** , **Postgres** , and **Elasticsearch** nodes.
1. Obtain Bastion host server details from your system administrator.

1. Ensure the following network infrastructure is available:

   1. Linux or Centos 64 bit operating system available.
   1. A Bastion host has the necessary 4 GB memory, 100 GB hard disk, and ports 22 and 9631 publicly accessible.
   1. PostgreSQL instance of _t3.medium_ type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 150 volume IOPS (input/output operations per second).
   1. Elasticsearch instance of _m5.large_ type with 16GB RAM for production (8 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 300 volume IOPS.
   1. Chef Automate instance of _t3.medium_ type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume IOPS.
   1. Chef Infra Server instance of _t3.medium_ type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume IOPS.

1. Ensure the following ports are open:

   | Habitat gossip (UDP), 9638 | Habitat http API, 9631 |
   | --- | --- |
   | PostgreSQL, 5432 | Pgleaderchk, 6432 |
   | HaProxy, 7432 | Elasticsearch (https), 9200 |
   | Elasticsearch (transport), 9300 | Kibana, 5601 |
   | Automate, ES-Node, 22,443 | |

1. Login as a root in the Bastion host.
1. Ensure you have _Chef Automate_ utility installed, else download and install the latest version.
1. Execute the command, _./chef-automate init-config-ha existing\_infra_, that generates **config.toml** file.
1. In the **config.toml** file, specify the list of VM's public IP addresses for the cluster.
1. Execute the command, **./chef-automate deploy config.toml** , that creates deployment workspace (\*/hab/a2\_deploy\_workspace\*), downloads Habitat, and establishes the cluster provisioning in your workspace.
1. Specify the following edits in the **config.toml** file:

   1. SSH pair name, key file path, chef automate nodes, number of PostgreSQL nodes, number of Chef Server, and ElasticSearch nodes.
   1. Provide load balancer URL as FQDN (Fully Qualified Domain Name).

1. Deploy and provision the Chef Automate HA.

## AWS Deployment

1. Set the software and hardware requirements.
1. Access or obtain an AWS account.

1. Ensure the following network infrastructure is available in your AWS account:
   1. Linux or Centos 64 bit operating system available.
   1. A bastion host has the necessary 4 GB memory, 100 GB hard disk, and ports 22 and _9631_ publicly accessible.
   1. PostgreSQL instance of _t3.medium_ type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 150 volume IOPS (input/output operations per second).
   1. Elasticsearch instance of _m5.large_ type with 16GB RAM for production (8 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 300 volume IOPS.
   1. Chef Automate instance of _t3.medium_ type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume IOPS.
   1. Chef Infra Server instance of _t3.medium_ type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume IOPS.

1. Setup Virtual Private Cloud (VPC) in AWS.

1. Build an AWS bastion host using the **AWS EC2 instance** option.
   1. Specify instance type as t2.medium, vCPUs as 1, Memory (GiB) as 4, and Instance Storage (GB) as EBS only.
   1. Change **VPC** and **subnet** values as required.
   1. Specify 100 GB of storage in the **Size** (GiB) field.
   1. Create a new security group or Select an existing security group option. Ensure Type is SSH, Protocol is TCP, and Port Range is 22 to create rules and connections.
   1. Launch the EC2 instance.

1. Ensure you have Chef Automate utility installed, else download and install the latest version.
1. Establish an AWS connection with the bastion host.
    1. SSH your instance using public DNS.

1. Create an IAM user using your AWS account.
   1. Provide the Programmatic access to the created user.
   1. Attach the existing policy directly.
   1. Provide Administrator access policy to the user.
   1. Download and save the access key and secret key.

1. Configure the AWS Credential on the bastion host.
   1. SSH into the bastion host.
   1. Create a directory **.aws** in /root folder.
   1. Create file **credentials** in the _/root/.aws_ directory.
   1. Touch _~/.aws/credentials_.
   1. Add the access key ID and secret key to the credentials file:
      - aws\_access\_key\_id=access key id of the IAM user
      - aws\_secret\_access\_key=secret access key of the IAM user.

1. Create the certificate for the Chef Automate and Chef Server load balancers.
1. Login as a root in the Bastion host.
1. Execute the command, _`./chef-automate init-config-ha aws_, which generates **config.toml** file with default settings and installs latest deployment package.
1. Execute the command, _./chef-automate provision-infra config.toml_, which downloads Habitat, creates deployment workspace (_/hab/a2\_deploy\_workspace_), and provisions the infrastructure on AWS.

1. Specify the following edits in the **config.toml** file:
   1. SSH pair name, key file path, chef automate nodes, number of PostgreSQL nodes, number of Chef Server, and ElasticSearch nodes.
   1. Attach the DNS certificate ARN to Chef Server load balancer certificate ARN (_automate\_lb\_certificate\_arn_)and Chef Automate load balancer certificate ARN (_chef\_server\_lb\_certificate\_arn_).
1. Deploy and provision the chef automate HA.
