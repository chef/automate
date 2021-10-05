+++
title = "High Availability"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "High Availability"
    parent = "automate/High_Availability"
    identifier = "automate/reference/High_Availability.md High Availability"
    weight = 50
+++

# Introduction

## What is High Availability (HA)?

High availability (HA) refers to a system (such as a network, a server array, or cluster) that offers a high level of operational performance and quality over a relevant time. They are designed to avoid loss of service by reducing or managing failures and minimizing planned downtime.

*This para required* \-- ? "Availability" includes two periods of time: how much time a service is accessible and how much time the system needs
to respond to user requests. When it comes to measuring availability, several factors are salient. These include recovery time and both scheduled and unscheduled maintenance periods.

Typically, availability as a whole is expressed as a percentage of uptime defined by service level agreements (SLAs). A score of 100
percent characterizes a system that never fails.

## What are HA clusters?

HA clusters are servers grouped to operate as a single, unified system. They are called failover clusters as they share the same storage but use
a different network and can run the same workloads of the primary system they support. HA clusters are tested regularly to confirm nodes are always in operational mode.

If a server in the cluster fails, another server or node can take over immediately to help ensure the application or service supported by the
cluster remains operational. HA clusters help ensure there is no single point of failure for critical IT and reduces or eliminates downtime.

# Chef Automate High Availability (HA) 

## What is Chef Automate Cluster?

Chef Automate Cluster is provided as a professional services solution offering installation, high availability, system uptime/ scale-out
performance, maintenance, and disaster recovery capabilities. It includes the Chef Server API to simplify the Chef Infrastructure.

These clusters are designed for those customers who have more than 10,000 chef client-nodes. You can configure it in the private data
center or preferred cloud.

The Chef Automate clusters comprise **four** different servers with HA mode, which are as follows:

1.  Chef-automate
2.  Chef-server
3.  Elasticsearch
4.  PostgreSQL

## What Chef Automate HA Brings to you?

The following architecture diagram shows the components involved in the Chef Automate HA.

![Graphical user interface, application Description automatically
generated](media/image1.png){width="4.730109361329834in"
height="2.495544619422572in"}

![High Availability Architecture](/images/automate/HA_Architecture.png)

Load balancer aids in identifying possible failure points and thereby helps in reducing downtime. Typically, availability as a whole is
expressed as a percentage of uptime. A highly available load balancer can achieve optimal operational performance through either a single-node deployment or through a deployment across a cluster. In a single-node deployment, a single load-balancing controller performs all administrative functions, and all analytics data gathering and processing. In a high availability load balancing cluster, additional nodes provide node-level redundancy for the load-balancing controller and maximize performance for CPU-intensive analytics functions.

In Chef Automate HA involves two different clusters in the main cluster, which are:

-   Automate Backend Cluster

The backend components are connected into the same habitat supervisor cluster. In the habitat supervisor, **postgres** and **elasticsearch** instances are running. Chef Automate HA model works on **Leader-Follower** strategy. For **Postgres** and **Elasticsearch** databases, a minimum of three nodes is required. Among these three, one is a leader, and another is a follower.

-   Automate Frontend Cluster

Chef Automate and Chef Server are considered as frontend nodes and serve a web UI. These components individually are configured with a load balancer.

**Journalbeat** and **metricbeat** are common for all database instances. **Journalbeat** collects all the services logs while
**metricbeat** collects system logs and sends them to the **Kibana**. **Kibana** runs in an **elasticsearch** server, and the related metrics
is viewed from the **Kibana** **Dashboard**.

## Performance and Scalability of Chef Automate

### What is Scalability -- Is this required?

Scalability refers to the ability of an application to handle the increase in workload or expand in response to an increased demand for
database access, processing, networking, or system resources.

### What is Performance -- Is this required?

Performance means system throughput under a given workload for a specific timeframe. It is validated by testing the scalability and the
reliability of hardware, software, and network. It is an ongoing process and not an end result. Performance requirements undergo massive changes
as features and functionalities are added and eliminated to accommodate evolving business requirements.

Following guidelines are adhered to achieve a robust degree of high availability on Chef Automate:

-   Chef Automate Cluster allows customers to have network infrastructure comprising more than 10,000 nodes while maintaining
    high performance and scalability.

-   Proper measures are employed while designing scalability and performance so that Chef Automate ingests reporting data from Chef
    Infra nodes and Chef InSpec reports efficiently.

-   Ingesting a report uses several services as the critical path of the Chef data must be monitored and tuned in order to reduce the data
    processing bottlenecks.

-   Several methods are followed in performance tuning, hardware planning, data retention policies, storage, network, load-balancing,
    and firewall services.

-   As Chef Automate Cluster is offered as a Chef Professional Service, the customer receives expert architecture planning, system
    architecture recommendations, and performance tuning for their estate.

## What happens during a failover?

HA is defined as the ability for the system to continue functioning after the failure of one or more of the servers. A part of HA is
failover that refers to the ability for client connections to migrate from one server to another in the event of server failure so client applications can continue to operate.

Chef Automate Cluster allows you to minimize downtime by utilizing redundant systems in case of a failure or maintenance. It includes HA
capabilities for Automate web services and the associated database services, which are:

-   Automate Frontend

Both Chef Automate and Chef Server have a load balancer with a UI. For example, let's say we have three chef-automate and chef-server instances. If any of the Chef Automate or Chef-Server instances becomes down, the traffic is distributed between the rest of the two servers.
Thus, the customer will not experience any downtime.

-   Automate Backend - check

The Elasticsearch and **Postgresql** database instances are considered as an automated backend. Chef habitat's hab supervisor concept is used
to make a cluster for database instance. Automate backend cluster rests in the habitat ring. For **postgresql**, **pgleaderchk** service runs in
all the **postgresql** instances and ensures to choose a leader in case of leader database failure. For **elasticsearch**, there is a **msae**
concept of leader follower. In case of any databse failure, a leader election occurs, and a new leader is chosen.

## Disaster Recovery (DR)

Disaster Recovery (DR) is a comprehensive plan for the recovery of critical operations and systems after catastrophic events. High
availability is focused on serious but more typical failures, such as a failing component or server. A DR plan may cope with the loss of an entire region, for example, although both are related.

DR cluster can be implemented on Chef Automate through a regular backup and restore measures syncing the data from the production cluster to the
DR cluster. Typically, these two clusters are located in different data centers or cloud provider regions. This is intended to provide an environment that can be made production in a short period with minimal data loss.

# System & Software Requirements 

## For platform support 

+----------------------+-----------------------------------------------+
|      Operating       |                               Tested          |
| Systems              | Versions                                      |
+======================+===============================================+
| Red Hat Enterprise   |  7, 8 (For 8 or above                         |
| Linux                | versions, **SELinux** configuration must be   |
|                      | permissive. By default, in RHEL               |
| (64 Bit OS)          | 8, **SELinux** configuration is enforced).    |
|                      |                                               |
|                      |                                               |
|                      |                                               |
|                      | Red Hat Enterprise Linux derivatives include  |
|                      | Amazon Linux v1 (using RHEL 6 packages) and   |
|                      | v2 (using RHEL 7 packages).                   |
+----------------------+-----------------------------------------------+
| Ubuntu (64 Bit OS)   |  14.04.x, 16.04.x, 18.04.x, 20.04.x           |
+----------------------+-----------------------------------------------+
| Centos (64 Bit OS)   |  7                                            |
+----------------------+-----------------------------------------------+

## Virtual Machine (VM) instances Type 

+--------------+-------------+--------------------------+--------------+
|              |       Type  |                    RAM   |              |
| Instance     |             |                          | Volume-size  |
+==============+=============+==========================+==============+
| PostgreSQL   | t3a.medium  | 4GB RAM For Test and 8G  | 50GB         |
|              |             | For Prod                 | (dedicated   |
|              |             |                          | hard disk    |
|              |             |                          | space need   |
|              |             |                          | to be        |
|              |             |                          | assigned to  |
|              |             |                          | '/')         |
+--------------+-------------+--------------------------+--------------+
| El           | m5a.large   | 8GB RAM For Test and 16G | 50G          |
| asticsearch  |             | For Prod                 | B (dedicated |
|              |             |                          | hard disk    |
|              |             |                          | space need   |
|              |             |                          | to be        |
|              |             |                          | assigned to  |
|              |             |                          | '/')         |
|              |             |                          |              |
|              |             |                          |              |
+--------------+-------------+--------------------------+--------------+
| Automate     | t3a.medium  | 4GB RAM For Test and 8G  | 50G          |
|              |             | For Prod                 | B (dedicated |
|              |             |                          | hard disk    |
|              |             |                          | space need   |
|              |             |                          | to be        |
|              |             |                          | assigned to  |
|              |             |                          | '/')         |
|              |             |                          |              |
|              |             |                          |              |
+--------------+-------------+--------------------------+--------------+
| Chef Server  | t3a.medium  | 4GB RAM For Test and 8G  | 50G          |
|              |             | For Prod                 | B (dedicated |
|              |             |                          | hard disk    |
|              |             |                          | space need   |
|              |             |                          | to be        |
|              |             |                          | assigned to  |
|              |             |                          | '/')         |
|              |             |                          |              |
|              |             |                          |              |
+--------------+-------------+--------------------------+--------------+

**Notes**:

ES volume size also depends on the number of nodes and frequency of client runs and compliance scans. The above table includes AWS instances
type, and however for bare infra deployment type or in premises deployment type, you can choose the above requirements for VM like RAM. 

**VPC Limit** 

The default limit to create a VPC in a region is 5. AWS Deployment creates two VPC, one for the bastion host and another for the rest of
the nodes in a cluster. For **ElasticSearch** and **postgres-sql**, a minimum of three node clusters are required. 

# A2HA Components -- need to clarify

This section lists the various Chef Automate HA components and its purpose.

### Automate-cluster-ctl 

Provides commands such as '*automate-cluster-ctl provision/deploy'*, which is installed via automate-backend-deployment.

### Automate-backend-ctl 

Aids in connecting the backend (**postgres** and **elasticsearch**) databases using an automate configuration file and **Terraform** without
any manual intervention.

### Automate-backend-curator 

Elasticsearch curator aids in curating and managing the Elasticsearch indices and snapshots by obtaining the entire actionable list of indices
(or snapshots) from the cluster. This component is same as the default curator. Its written in a **hab** package to merge application in a hab
environment.

### Automate-backend-deployment 

Aids in setting up a workspace for a2ha environment. For example, */hab/a2_deploy_workspace*. It also includes **terraform** code, some
necessary **scripts**, **inspecs**, **tests**, **Makefile** and so on.

###  Automate-backend-elasticsearch 

Includes the **elasticsearch** configuration and builds the **elasticsearch** package. It is installed in the backend nodes.

### Automate-backend-elasticsidecar \<\<check>\>

Provides a sidecar service for **automate-backend-elasticsearch** that reads users credentials and passwords of the **elasticsearch** binding
and applies it to Elasticsearch using the **odfe** tooling.

### Automate-backend-haproxy 

Aids in sending a request to the leader node and is placed on **postgres** cluster.

### Automate-backend-kibana 

Aids in viewing logs at a central place. The **Kibana** **Dashboard** displays the **postgres** and **elasticsearch** nodes system metrics,
and services logs.

### Automate-backend-journalbeat 

Aids in collecting **journalctl** logs like all the service logs. It is placed on all **postgres** and **elasticsearch** nodes. It collects and
sends the log information to **elasticsearch** and the **Kibana** **Dashboard** displays the respective log information..

### Automate-backend-metricbeat 

This component is placed on all **postgres** and **elasticsearch** nodes. It collects all related metrics and sends them to
**elasticsearch.** The **Kibana** **Dashboard** displays the respective log information.

### Automate-backend-pgleaderchk 

This component is used in a proxy health check to determine where to route SQL requests. A **golang** service that checks the local
**PostgreSQL** instance to view if it\'s a *Leader*.

### Automate-backend-postgresql 

This component is a wrapper package of *core/postgresql11* for Chef Automate that provides a backend **HA PostgreSQL**.

# Reference Architecture

This section includes high-level reference implementations of a2-ha-backend on different providers, or in different kinds of
environments. 

## Type of Deployment Support 

Currently, Chef Automate HA supports two types of deployment, which are

1.  AWS deployment 

2.  Bare Infrastructure Deployment (existing_node) 

### AWS Deployment 

In AWS deployment, the entire a2ha infrastructure is created into the AWS cloud. If you choose AWS as a reference architecture, there is a standard **terraform** script that takes care of AWS deployment. This deployment terraform script first sets up all the prerequisites like creating a VPC, EC2, Load Balancer, Security Groups. After that, configuration and installation takes place like installing automate into the automate instances, installing chef server in all chef-server instances, installing and configuring **postgresql** into the **postres** instances, and lastly, configuring and installing **elasticsearch** into **elasticsearch** instances. There are many other installation and configurations happens during deployment like installing a habitat and creation of a supervisor network.

### Bare Infrastructure Deployment / On premises (existing_node) 

Some customers already have basic network infrastructure with VMs, network, load balancer in their environment. Sometimes it can be on-premises or in the cloud. Some organizations don't want to provide access to create items like VMs and in such cases IPs of their instances are used.

If you choose bare infrastructure or on premises reference architecture, **Terraform** creates all components from scratch like VPC, ec2, Load
Balancer. If you don't let terraform create them, or the customer have already created those by themselves, or customers are having on premises servers and they just want to configure a2ha (**automate**, **chef-server**, **elasticsearch**, **postgresql**) in those servers, then the customer should choose existing_node reference architecture. There is also a terraform script for this scenario but that script only takes care of installing and configuring components and not creating instances in the cloud providers.

# Bastion Setup/system

## Description

## Creation and details

# Topology (Details on components- introduction and details around that)
