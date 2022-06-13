+++
title = "Chef Automate High Availability"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Chef Automate High Availability"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha.md Chef Automate High Availability"
    weight = 200
+++

**High availability (HA)** refers to a system or application that offers a high level of operational availability. This means that if one server goes down, whether due to traffic overload or other issues, the entire site or application will not be down. HA means the application remains available with no interruption. We achieve high availability when an application continues to operate even when one or more underlying components fail.

Thus, HA is designed to avoid loss of service by reducing or managing failures and minimizing unscheduled downtime (when your system or network is not available for use or is unresponsive) that happens due to power outages or failure of a component.


## Chef Automate High Availability (HA)

The Chef Automate HA equates to reliability, efficiency, and productivity. Chef Automate HA is built on the following characteristics, **Redundancy**, and **Failover**. It aids in addressing major issues like service failure, and zone failure.

## Chef Automate HA Architecture
The following Chef Automate HA Architecture diagram shows the Chef Automate HA components. This architecture includes the cluster of the _Chef Automate_, _Chef Server_, _Postgres_, and _OpenSearch_.

## Chef Automate HA Topology

The Chef Automate HA Architecture involves the following clusters part of the main cluster:

- **Backend Cluster**

- **Postgres:** Database requires a minimum of three nodes. Postgres database uses *Leader-Follower* strategy, where one becomes a leader, and the other two are the followers.

- **OpenSearch:** Database requires a minimum of three nodes. OpenSearch database manages the [cluster internally](https://opensearch.org/docs/latest/opensearch/cluster/).

- **Frontend Cluster**

Chef Automate and Chef Server act as frontend nodes and serve as a web UI with Load Balancer configurations.

- [Chef Automate](https://docs.chef.io/automate/)

- [Chef Server](https://docs.chef.io/server/)

## Deployment Methods

Chef Automate High Availability (HA) supports two types of deployment:

- [On-premise Deployment (Existing Node) Deployment](/automate/ha_deploy_bareinfra)
- [Amazon Web Services (AWS) Deployment](/automate/ha_auto_install)


### On-premise Deployment (Existing Node/Bare Infrastructure)

![High Availability Architecture](../../static/images/automate/ha_arch_onpremise.png)
![High Availability Architecture](/images/automate/ha_arch_onpremise.png)

A **Bare Metal computer** is generally without any software (OS or applications). However, when contrasted with a virtualized server environment, bare metal may imply a regular, non-virtual server that does include an OS.

A bare-metal server is a non-shared computer dedicated to one customer in cloud computing. It generally implies a non-virtual machine (VM) environment. The difference between bare metal servers and cloud servers is that a cloud server is a virtual machine. In contrast, a bare metal server is a physical machine identified within a data center.

Bare Infrastructure deployments are operating system installations to targets that either have no operating system installed or must be re-installed without preserving any existing data or settings. You can install and manage Chef Automate HA by creating profiles for bare metal deployments.

Some customers already have basic network infrastructure with VMs, networks, and load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not want to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

As an AWS setup, **Terraform** creates all components from scratch, like EC2 and Load Balancer. If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers want to configure Chef Automate HA (**automate**, **chef-server**, **opensearch**, **postgresql**) in HA servers, then the customer should choose **Existing Node** reference architecture.

You can also execute the **Terraform** script for the bare infra deployment scenario. However, this script only handles installing and configuring components and does not create instances on the cloud providers.
### Cloud Deployment using Amazon Web Services (AWS)

![High Availability Architecture](../../static/images/automate/ha_arch_aws_managed.png)
![High Availability Architecture](/images/automate/ha_arch_aws_managed.png)

AWS is a comprehensive, evolving cloud computing platform provided by Amazon that includes a mixture of infrastructure as a service (IaaS), platform as a service (PaaS), and packaged software as a service (SaaS) offerings. AWS services can offer an organization tools such as compute power, database storage, and content delivery services. Click [here](https://aws.amazon.com/what-is-cloud-computing/) to learn more.

The entire Chef Automate HA infrastructure is built into the AWS cloud in AWS deployment. A standard **Terraform** script handles AWS deployment if you choose AWS as a reference architecture. This deployment terraform script sets up all the prerequisites like creating an EC2, load balancer, security groups, and subnets.

The two step deployment process is as shown below:

- Provisioning Infrastructure.
- Deployment of services on provisioned infrastructure.
  - Installation of *PostgreSQL*, *OpenSearch*, *Chef Automate*, *Chef Server* will be done in this step.
