+++
title = "High Availability - System and Software Requirements"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "High Availability - System and Software Requirements"
    parent = "automate/install"
    identifier = "automate/install/ha_system_requirements.md High Availability - System and Software Requirements"
    weight = 240
+++

This section lists the recommended operating systems requirements, virtual machine instances requirements, and VPC requirements for implementing Chef Automate High Availability (HA) for your network infrastructure or systems or applications or services.

## Architecture Reference

The following Chef Automate HA architecture diagram shows the components involved in the Chef Automate HA that works on the **Leader-Follower** strategy. We are creating the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch for Chef Automate HA.

![High Availability Architecture](/images/automate/ha_architecture.png)

All application service, secret, recovery, data are stored in the PostgreSQL. Compliance and client run data that are generated on time to time basis and requiring data to be accessible in real-time are stored in Elastic search. Load balancer distributes to each of the automate components.

**Journalbeat** and **Metricbeat** are common for all database instances. **Journalbeat** installed as an agent on the servers collects all the services logs and forwards them to Elasticsearch. **Metricbeat** installed on the servers periodically collects metrics from the operating system and services running on the server and sends them to the **Kibana**.

**Kibana** is an open-source, web-based data visualization and analytical tool that allows you to explore, visualize, and build a dashboard over the log data massed in Elasticsearch clusters. It is a part of the Elastic Stack and integrates with Elasticsearch. The **Kibana** **Dashboard** is a collection of charts, graphs, metrics, searches, and maps in a single pane and provides at-a-glance insights into data from multiple perspectives enabling you to drill down into the details.

### Automate Clusters

The Chef Automate HA architecture involves two different clusters part of the main cluster, which are:

- Automate Backend Cluster and Nodes

The backend components connect into the frontend habitat supervisor cluster. In the habitat supervisor, **postgres** and **elasticsearch** instances runs. A minimum of three nodes is required for **Postgres** and **Elasticsearch** databases, where one becomes a leader, and others are followers.

- Automate Frontend Cluster and Nodes

Chef Automate and Chef Server act as frontend nodes and serve as a web UI with load balancer configurations.

### Automate Cluster Nodes

The backend and frontend clusters comprise **four** different servers with HA mode, which are as follows:

1. Chef-automate
2. Chef Infra Server
3. Elasticsearch - an open-source search and analytics engine based on Apache Lucene and built with Java. It is a time-series and NoSQL database that stores data in an unstructured way and is used for indexing purposes.
4. PostgreSQL - an open-source relational database management system (RDBMS) emphasizing extensibility and SQL compliance.

<!-- ! -- These four components reside in a VPC under one network in AWS. Every node sits on a specific machine irrespective of a database. Single database for all three nodes of automate. -->

{{ note >}}

Elastic Search internally manages the communication and backup, and does not follow any leader-follower strategy.

{{ /note >}}

### Deployment Methods

Currently, Chef Automate High Availability (HA) supports two types of deployment, which are

#### Amazon Web Services (AWS) Deployment

AWS is a comprehensive, evolving cloud computing platform provided by Amazon that includes a mixture of infrastructure as a service (IaaS), platform as a service (PaaS), and packaged software as a service (SaaS) offerings. AWS services can offer an organization tools such as compute power, database storage, and content delivery services. Learn more @ <https://aws.amazon.com/what-is-cloud-computing/>.

In AWS deployment, the entire Chef Automate HA infrastructure is built into the AWS cloud. If you choose AWS as a reference architecture, a standard **Terraform** script handles AWS deployment. This deployment terraform script first sets up all the prerequisites like creating an EC2, load balancer, security groups, subnets. Then, ensure the existing **VPCID** is provided for security purposes, and the **cidr** block is created manually based on respective **VPC**.

Its a standard cloud services. Later, a series of configurations and installation follows:

- installing automate into the automate instances
- installing Chef Infra Server in all chef-server instances
- installing and configuring **PostgreSQL** into the **postres** instances
- configuring and installing **Elasticsearch** into **elasticsearch** instances, and
- installing a Chef Habitat and creating a supervisor network.

#### Bare Infrastructure / On-premise Deployment (existing_node) Deployment Method

A **Bare Metal computer** is generally without any software (OS or applications). However, when contrasted with a virtualized server environment, bare metal may imply a regular, non-virtual server that does include an OS.

In cloud computing, a *bare-metal* server is a non-shared computer dedicated to one customer. It generally implies a non-virtual machine (VM) environment. The difference between bare metal servers and cloud servers is that a cloud server is a virtual machine while the bare metal server is a physical machine identified within a data center.

Bare Metal deployments are installations of operating systems to targets that either has no operating system installed, or must be re-installed without preserving any existing data or settings. You can install and manage Chef Automate HA by creating profiles for bare metal deployments.

Some customers already have basic network infrastructure with VMs, networks, load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not want to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

As an AWS setup, **Terraform** creates all components from scratch like EC2, Load Balancer. If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers just want to configure Chef Automate HA (**automate**, **chef-server**, **elasticsearch**, **postgresql**) in those servers, and then the customer should choose existing_node reference architecture.

You can also utilize **Terraform** script for the bare infra deployment scenario. However, this script only handles installing and configuring components and does not create instances on the cloud providers.
