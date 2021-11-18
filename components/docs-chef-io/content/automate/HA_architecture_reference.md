+++
title = "Chef Automate HA - Reference Architecture"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - Reference Architecture"
    parent = "automate/High_Availability"
    identifier = "automate/reference/ha_architecture_reference.md HA - Reference Architecture"
    weight = 20
+++

## Reference Architecture

This section includes Chef Automate High Availability (HA) high-level reference architecture that interacts with the HA backend components on different providers or in different environments.

The following Chef Automate HA architecture diagram shows the components involved in the Chef Automate HA that works on **Leader-Follower** strategy. We are creating the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch for Chef Automate HA.

![High Availability Architecture](/images/automate/HA_Architecture.png)

The Chef Automate HA architecture involves two different clusters part of the main cluster, which are:

- Automate Backend Cluster and Nodes

The backend components connect into the frontend habitat supervisor cluster. In the habitat supervisor, **postgres** and **elasticsearch** instances runs. A minimum of three nodes is required for **Postgres** and **Elasticsearch** databases, where one becomes a leader, and others are followers.

- Automate Frontend Cluster and Nodes

Chef Automate and Chef Server act as frontend nodes and serve as a web UI with load balancer configurations.

These clusters comprise **four** different servers with HA mode, which are as follows:

1. Chef-automate
2. Chef Infra Server
3. Elasticsearch - an open-source search and analytics engine based on Apache Lucene and built with Java. It is a NoSQL database that stores data in an unstructured way.
4. PostgreSQL - an open-source relational database management system (RDBMS) emphasizing extensibility and SQL compliance.

{{ note >}}

Elastic Search internally manages the communication and backup, and  does not follow any leader-follower stratery.

{{ /note >}}

**Journalbeat** and **Metricbeat** are common for all database instances. **Journalbeat** installed as an agent on the servers collects all the services logs and forwards them to Elasticsearch. **Metricbeat** installed on the servers periodically collects metrics from the operating system and services running on the server and sends them to the **Kibana**.

**Kibana** is an open-source, web-based data visualization and analytical tool that allows you to explore, visualize, and build a dashboard over the log data massed in Elasticsearch clusters. It is a part of the Elastic Stack and integrates with Elasticsearch. The **Kibana** **Dashboard** is a collection of charts, graphs, metrics, searches, and maps in a single pane and provides at-a-glance insights into data from multiple perspectives enabling you to drill down into the details.

## Deployment Support Types

Currently, Chef Automate HA supports two types of deployment, which are

1. AWS deployment
2. Bare Infrastructure Deployment (existing_node)

### AWS Deployment

In AWS deployment, the entire Chef Automate HA infrastructure is built into the AWS cloud. If you choose AWS as a reference architecture, a standard **Terraform** script handles AWS deployment. This deployment terraform script first sets up all the prerequisites like creating a VPC, EC2, load balancer, security groups, subnets. Then, ensures the **VPCID** is provided for security purposes, and the **cidr** block is created manually based on respective **VPC**.

Later, following series of configurations and installation happens:

- installing automate into the automate instances
- installing Chef Infra Server in all chef-server instances
- installing and configuring **PostgreSQL** into the **postres** instances
- configuring and installing **Elasticsearch** into **elasticsearch** instances, and
- installing a Chef Habitat and creation of a supervisor network.

### Bare Infrastructure Deployment / On-premise Deployment (existing_node)

Some customers already have basic network infrastructure with VMs, networks, load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not wanting to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

#### What happens if you choose bare infrastructure or on-premises reference architecture?

As a AWS setup **Terraform** creates all components from scratch like VPC, EC2, Load Balancer if you choose bare infrastructure or on-premises reference architecture.

If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers just want to configure Chef Automate HA (**automate**, **chef-server**, **elasticsearch**, **postgresql**) in those servers, and then the customer should choose existing_node reference architecture. 

You can also utilize **Terraform** script for this scenario; however, then this script only handles installing and configuring components and does not create instances on the cloud providers.
