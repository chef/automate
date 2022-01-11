+++
title = "HA Deployment Methods"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA Deployment Methods"
    parent = "automate/install"
    identifier = "automate/install/ha_deploy_methods.md HA Deployment Methods"
    weight = 200
+++

## Deployment Methods

Currently, Chef Automate High Availability (HA) supports two types of deployment, which are

1. Amazon Web Services (AWS) Deployment. Refer [Chef Automate HA Deployment using AWS]({{< relref "ha_deploy_aws.md" >}}) section for building an bastion host and Chef Automate HA deployment procedure using AWS.
2. Bare Metal Infrastructure Deployment (existing_node). Refer [Chef Automate HA Deployment for Bare Infra]({{< relref "ha_deploy_bareinfra.md" >}}) section for bare infra Chef Automate HA deployment procedure.

### AWS Deployment Method

The standard terraform script deploys the cluster of automate, chef-server, elastic-search and postgress on AWS cloud. This script includes steps that creates VPC (Amazon Virtual Private Cloud), security-group, setting up the EC2 (Amazon Elastic Compute Cloud) and so on.

Is this required?

AWS is a comprehensive, evolving cloud computing platform provided by Amazon that includes a mixture of infrastructure as a service (IaaS), platform as a service (PaaS) and packaged software as a service (SaaS) offerings. AWS services can offer an organization tools such as compute power, database storage and content delivery services. Learn more @ <https://aws.amazon.com/what-is-cloud-computing/>.

In AWS deployment, the entire Chef Automate HA infrastructure is built into the AWS cloud. If you choose AWS as a reference architecture, a standard **Terraform** script handles AWS deployment. This deployment terraform script first sets up all the prerequisites like creating a EC2, load balancer, security groups, subnets. Then, ensure the existing **VPCID** is provided for security purposes, and the **cidr** block is created manually based on respective **VPC**.

Its a standard cloud services.

Later, series of configurations and installation follows:

- installing automate into the automate instances
- installing Chef Infra Server in all chef-server instances
- installing and configuring **PostgreSQL** into the **postres** instances
- configuring and installing **Elasticsearch** into **elasticsearch** instances, and
- installing a Chef Habitat and creation of a supervisor network.

### Bare Infrastructure / On-premise Deployment (existing_node) Deployment Method

 A **Bare Metal computer** is generally one without any software (OS or applications). However, when contrasted with a virtualized server environment, bare metal may imply a regular, non-virtual server that does include an OS.

In cloud computing, a *bare-metal* server is a non-shared computer dedicated to one customer. It generally implies a non-virtual machine (VM) environment.

The difference between bare metal servers and cloud servers is that cloud server is a virtual machine while the bare metal server is a physical machine identified within a data center.

Bare Metal deployments are installations of operating systems to targets that either have no operating system installed, or must be re-installed without preserving any existing data or settings.

You can install and manage Chef Automate HA by creating profiles for bare metal deployments.

Some customers already have basic network infrastructure with VMs, networks, load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not wanting to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

As a AWS setup, **Terraform** creates all components from scratch like EC2, Load Balancer. If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers just want to configure Chef Automate HA (**automate**, **chef-server**, **elasticsearch**, **postgresql**) in those servers, and then the customer should choose existing_node reference architecture.

You can also utilize **Terraform** script for bare infra deployment scenario. However, then this script only handles installing and configuring components and does not create instances on the cloud providers.
