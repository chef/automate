+++
title = "HA - Reference Architecture"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - Reference Architecture"
    parent = "automate/High_Availability"
    identifier = "automate/reference/HA_architecture_reference.md HA - Reference Architecture"
    weight = 20
+++

## Reference Architecture

This section includes Chef Automate High Availability (HA) high-level reference architecture that interacts with the HA backend components on different providers or in different environments.

## Deployment Support Types

Currently, Chef Automate HA supports two types of deployment, which are

1. AWS deployment

2. Bare Infrastructure Deployment (existing_node)

### AWS Deployment

In AWS deployment, the entire Chef Automate HA infrastructure is built into the AWS cloud. If you choose AWS as a reference architecture, a standard **terraform** script handles AWS deployment. This deployment terraform script first sets up all the prerequisites like creating a VPC, EC2, load balancer, security groups, subnets. Ensure the **vpcid** is provided for security purposes and the **cidr** block is created manually based on respective **vpc**.

Later, series of configurations and installation happens like:

- installing automate into the automate instances
- installing Chef Infra Server in all chef-server instances
- installing and configuring **PostgreSQL** into the **postres** instances
- configuring and installing **Elasticsearch** into **elasticsearch** instances, and
- installing a habitat and creation of a supervisor network.

### Bare Infrastructure Deployment / On-premise Deployment (existing_node)

Some customers already have basic network infrastructure with VMs, networks, load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not wanting to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

#### What happens if you choose bare infrastructure or on-premises reference architecture?

If you choose bare infrastructure or on premises reference architecture, **Terraform** creates all components from scratch like VPC, ec2, Load Balancer.

If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers just want to configure Chef Automate HA (**automate**, **chef-server**, **elasticsearch**, **postgresql**) in those servers, and then the customer should choose existing_node reference architecture. You can also utilize **Terraform** script for this scenario; however, then this script only handles installing and configuring components and does not create instances on the cloud providers.
