+++
title = "HA Wrokflow"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA Workflow"
    parent = "automate/install"
    identifier = "automate/install/ha_workflow.md HA Workflow"
    weight = 200
+++

## Workflow

### Chef Automate HA Workflow – Bare Infra Deployment

1. Set the software and hardware requirements.
1. Obtain necessary virtual machine instance details (with public and private addresses) to create the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch nodes.
1. Obtain bastion host server details from your system administrator.
1. Ensure the following network infrastructure is available:
    - Linux or Centos 64 bit operating system available.
    - Bastion host has the necessary 4 GB memory, 100 GB hard disk, and ports 22 and 9631 publicly accessible.
    - PostgreSQL instance of t3.medium type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 150 volume iops (input/output operations per second).
    - Elasticsearch instance of m5.large type with 16GB RAM for production (8 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 300 volume iops (input/output operations per second).
    - Chef Automate instance of t3.medium type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume iops (input/output operations per second).
    - Chef Infra Server instance of t3.medium type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume iops (input/output operations per second).
1. Ensure you have running instance of Chef Automate utility.
1. Ensure following ports are open:
    - Habitat gossip (UDP), 9638
    - Habitat http API, 9631
    - PostgreSQL, 5432
    - Pgleaderchk, 6432
    - HaProxy, 7432
    - Elasticsearch (https), 9200
    - Elasticsearch (transport), 9300
    - Kibana, 5601
    - Automate,ES-Node, 22,443
1. Login as a root in the Bastion host.
1. Generate config.toml file by using command, ./chef-automate init-config-ha existing_infra.
1. In the config.toml file, specify the list of IP addresses for the cluster. If public IPs for the vm's arnt available, you can mention the respective private IP addresses.
1. Create deployment workspace (*/hab/a2_deploy_workspace*), download Habitat, and establish cluster provisioning in your workspace by using command, ./chef-automate deploy config.toml.
1. Specify the following edits in the config.toml file:
    - SSH pair name, key file path, chef automate nodes, chef infra server cluster nodes, cluster number for PostgreSQL, Chef Server and ElasticSearch, and secret management key.
    - Provide load balancer URL as FQDN.
1. Deploy and provision the chef automate HA.

### Chef Automate HA Workflow – AWS Deployment

1. Set the software and hardware requirements.
1. Ensure the following network infrastructure is available:
    - Linux or Centos 64 bit operating system available.
    - Bastion host has the necessary 4 GB memory, 100 GB hard disk, and ports 22 and 9631 publicly accessible.
    - PostgreSQL instance of t3.medium type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 150 volume iops (input/output operations per second).
    - Elasticsearch instance of m5.large type with 16GB RAM for production (8 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 300 volume iops (input/output operations per second).
    - Chef Automate instance of t3.medium type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume iops (input/output operations per second).
    - Chef Infra Server instance of t3.medium type with 8GB RAM for production (4 GB is enough for testing), 50 GB hard disk space, gp2 volume type, and 100 volume iops (input/output operations per second).
1. Access or obtain an AWS account.
1. Ensure you have running instance of Chef Automate utility, or download the latest package and install it.
1. Setup Virtual Private Cloud (VPC) in AWS.
1. Build an AWS bastion host using AWS EC2 instance option.
    - Specify instance type as t2.medium, vCPUs as 1, Memory (GiB) as 4 and Instance Storage (GB) as EBS only.
    - Modify VPC and subnet values as required.
    - Specify 100 GB of storage in Size (GiB) field.
    - Create a new security group or Select an existing security group option. Ensure Type is SSH, Protocol is TCP, and Port Range is 22 to create rules and connections.
1. Launch the EC2 instance.
1. Establish AWS connection with bastion host.
    - SSH your instance using public DNS.
1. Create an IAM user using your AWS account.
    - Provide the Programmatic access to the created user.
    - Attach the existing policy directly.
    - Provide Administrator access policy to the user.
1. Download and save the access key and secret key.
1. Configure the AWS Credential on the bastion host.
1. Create the certificate for the DNS.
1. Login as a root in the Bastion host.
1. Generate config.toml file with default settings by using command, `./chef-automate init-config-ha aws. Also, it installs latest deployment package.
1. Downloads Habitat, create deployment workspace (*/hab/a2_deploy_workspace*), and provision the infrastructure on AWS by using command, ./chef-automate provision-infra config.toml.
1. Specify the following edits in the config.toml file:
    - SSH pair name, key file path, chef automate nodes, chef infra server cluster nodes, cluster number for PostgreSQL, Chef Server and ElasticSearch, and secret management key.
    - Provide load balancer URL as FQDN. Use  the purchased licnese or the existing domain.
1. Deploy and provision the chef automate HA.
1. Create certificates. Communication inside chef n automate is https. Create record in route 53. Route is a DNS service available for AWS. Make a entry in route 53.
