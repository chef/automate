+++
title = "High Availability Architecture"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "High Availability Architecture"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha_architecture_reference.md High Availability Architecture"
    weight = 220
+++

The following Chef Automate HA Architecture diagram shows the Chef Automate HA components that work on the **Leader-Follower** strategy. This architecture includes the cluster of the _Chef Automate_, _Chef Server_, _Postgres_, and _OpenSearch_.

![High Availability Architecture](/images/automate/ha_architecture.png)

## Chef Automate Clusters

The Chef Automate HA Architecture involves the following clusters part of the main cluster, which are:

- Backend Cluster and Nodes
- Frontend Cluster and Nodes

### Backend Cluster and Nodes

The backend components connect to the frontend Chef Habitat supervisor cluster. The **Postgres** and **OpenSearch** instances run in the Chef Habitat supervisor. The **Postgres** and **OpenSearch** databases require a minimum of three nodes, where one becomes a leader, and the other two are followers.

### Frontend Cluster and Nodes

Chef Automate and Chef Server act as frontend nodes and serve as a web UI with load balancer configurations.

## Chef Automate Cluster Nodes

The backend and frontend clusters comprise **four** different servers with HA mode, which are as follows:

- **Chef-automate**
- **Chef Infra Server**
- **OpenSearch** is an open-source search and analytics engine based on Apache Lucene, built with Java. It is a time-series and NoSQL database that stores data in an unstructured way and can be used for indexing purposes.
- **PostgreSQL** is an open-source relational database management system (RDBMS) emphasizing extensibility and SQL compliance.

<!-- ! -- These four components reside in a VPC under one network in AWS. Every node sits on a specific machine irrespective of a database. Single database for all three nodes of Chef Automate. -->

**PostgreSQL** stores all application services, secret, recovery, and data.

**Open Search** stores compliance and client-run data and requires these data to be accessible in real-time. A Load balancer distributes to each of the Chef Automate components.

{{< note >}} Open Search internally manages the communication and backup and does not follow any leader-follower strategy. {{< /note >}}

## Deployment Methods

Chef Automate High Availability (HA) supports two types of deployment:

- [Amazon Web Services (AWS) Deployment](/ha_auto_install)
- [On-premise Deployment (Existing Node) Deployment](/ha_deploy_bareinfra)

### Cloud Deployment using Amazon Web Services (AWS)

AWS is a comprehensive, evolving cloud computing platform provided by Amazon that includes a mixture of infrastructure as a service (IaaS), platform as a service (PaaS), and packaged software as a service (SaaS) offerings. AWS services can offer an organization tools such as compute power, database storage, and content delivery services. Click [here](https://aws.amazon.com/what-is-cloud-computing/) to learn more.

The entire Chef Automate HA infrastructure is built into the AWS cloud in AWS deployment. A standard **Terraform** script handles AWS deployment if you choose AWS as a reference architecture. This deployment terraform script first sets up all the prerequisites like creating an EC2, load balancer, security groups, and subnets. Ensure you have given the existing **VPCID** for security purposes, and you have created the **CIDR** block manually based on the respective **VPC**.

It is a standard cloud service. Later, a series of configurations and installations follows:

- Installing Chef Automate into the Chef Automate instances.
- Installing Chef Infra Server in all chef-server instances.
- Installing and Configuring **PostgreSQL** into the **postres** instances.
- Configuring and Installing **OpenSearch** into **opensearch** instances.
- Installing a Chef Habitat and creating a supervisor network.

### On-premise Deployment (Existing Node/Bare Infrastructure)

A **Bare Metal computer** is generally without any software (OS or applications). However, when contrasted with a virtualized server environment, bare metal may imply a regular, non-virtual server that does include an OS.

A bare-metal server is a non-shared computer dedicated to one customer in cloud computing. It generally implies a non-virtual machine (VM) environment. The difference between bare metal servers and cloud servers is that a cloud server is a virtual machine. In contrast, a bare metal server is a physical machine identified within a data center.

Bare Infrastructure deployments are operating system installations to targets that either have no operating system installed or must be re-installed without preserving any existing data or settings. You can install and manage Chef Automate HA by creating profiles for bare metal deployments.

Some customers already have basic network infrastructure with VMs, networks, and load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not want to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

As an AWS setup, **Terraform** creates all components from scratch, like EC2 and Load Balancer. If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers want to configure Chef Automate HA (**automate**, **chef-server**, **opensearch**, **postgresql**) in HA servers, then the customer should choose **Existing Node** reference architecture.

You can also execute the **Terraform** script for the bare infra deployment scenario. However, this script only handles installing and configuring components and does not create instances on the cloud providers.
