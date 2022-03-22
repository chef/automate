+++
title = "Architecture and Components"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Architecture and Components"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_architecture_reference.md Architecture and Components"
    weight = 20
+++

This page explains the Chef Automate High Availability (HA) architecture, system requirements, and lists the HA components with their purpose.

## Chef Automate HA Architecture

The following Chef Automate HA architecture diagram shows the Chef Automate HA components that works on the **Leader-Follower** strategy. This architecture includes the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch.

![High Availability Architecture](/images/automate/ha_architecture.png)

**PostgreSQL** stores all application service, secret, recovery, and data. **Elastic Search** stores compliance and client-run data and requires these data to be accessible in real-time. A Load balancer distributes to each of the Chef Automate components.

**Journalbeat** and **Metricbeat** are common for all database instances. **Journalbeat** installed as an agent on the servers collects all the services logs and forwards them to Elasticsearch. **Metricbeat** installed on the servers periodically collects metrics from the operating system and services running on the server and sends them to the **Kibana**.

**Kibana** is an open-source, web-based data visualization and analytical tool that allows you to explore, visualize, and build a dashboard over the log data massed in Elasticsearch clusters. Kibana is a part of the Elastic Stack and integrates with Elasticsearch. The **Kibana Dashboard** is a collection of charts, graphs, metrics, searches, and maps in a single pane and provides at-a-glance insights into data from different perspectives enabling you to drill down into the details.

### Chef Automate Clusters

The Chef Automate HA architecture involves the following clusters part of the main cluster, which are:

- Backend Cluster and Nodes

The backend components connect into the frontend Chef Habitat supervisor cluster. The **postgres** and **elasticsearch** instances run in the Chef Habitat supervisor. The **Postgres** and **Elasticsearch** databases require a minimum of three nodes, where one becomes a leader, and the other two are followers.

- Frontend Cluster and Nodes

Chef Automate and Chef Server act as frontend nodes and serve as a web UI with load balancer configurations.

### Chef Automate Cluster Nodes

The backend and frontend clusters comprise **four** different servers with HA mode, which are as follows:

1. Chef-automate
2. Chef Infra Server
3. Elasticsearch - an open-source search and analytics engine based on Apache Lucene, built with Java. It is a time-series and NoSQL database that stores data in an unstructured way and is used for indexing purposes.
4. PostgreSQL - an open-source relational database management system (RDBMS) emphasizing extensibility and SQL compliance.

<!-- ! -- These four components reside in a VPC under one network in AWS. Every node sits on a specific machine irrespective of a database. Single database for all three nodes of automate. -->

{{ note >}}

Elastic Search internally manages the communication and backup and does not follow any leader-follower strategy.

{{ /note >}}

### Deployment Methods

Chef Automate High Availability (HA) supports two types of deployment, which are

#### Amazon Web Services (AWS) Deployment

AWS is a comprehensive, evolving cloud computing platform provided by Amazon that includes a mixture of infrastructure as a service (IaaS), platform as a service (PaaS), and packaged software as a service (SaaS) offerings. AWS services can offer an organization tools such as compute power, database storage, and content delivery services. Learn more @ <https://aws.amazon.com/what-is-cloud-computing/>.

The entire Chef Automate HA infrastructure is built into the AWS cloud in AWS deployment. A standard **Terraform** script handles AWS deployment if you choose AWS as a reference architecture. This deployment terraform script first sets up all the prerequisites like creating an EC2, load balancer, security groups, subnets. Ensure you have given the existing **VPCID** for security purposes, and you have created the **CIDR** block manually based on respective **VPC**.

Its a standard cloud service. Later, a series of configurations and installations follows:

- installing Chef Automate into the Chef Automate instances
- installing Chef Infra Server in all chef-server instances
- installing and configuring **PostgreSQL** into the **postres** instances
- configuring and installing **Elasticsearch** into **elasticsearch** instances, and
- installing a Chef Habitat and creating a supervisor network.

#### On-premise Deployment (Existing Node) Deployment

A **Bare Metal computer** is generally without any software (OS or applications). However, when contrasted with a virtualized server environment, bare metal may imply a regular, non-virtual server that does include an OS.

In cloud computing, a bare-metal server is a non-shared computer dedicated to one customer. It generally implies a non-virtual machine (VM) environment. The difference between bare metal servers and cloud servers is that a cloud server is a virtual machine while the bare metal server is a physical machine identified within a data center.

Bare Metal deployments are installations of operating systems to targets that either have no operating system installed or must be re-installed without preserving any existing data or settings. You can install and manage Chef Automate HA by creating profiles for bare metal deployments.

Some customers already have basic network infrastructure with VMs, networks, load balancers in their environment. This environment can be on-premises or in the cloud, and the respective organizations might not want to provide access to create items like VMs. In such cases, IPs of their instances are used to set up Chef Automate HA on their network premises.

As an AWS setup, **Terraform** creates all components from scratch like EC2, Load Balancer. If you don't let **Terraform** create them, or the customer has already made those by themselves, or customers have on-premises servers, or the customers wants to configure Chef Automate HA (**automate**, **chef-server**, **elasticsearch**, **postgresql**) in HA servers, then the customer should choose **Existing Node** reference architecture.

You can also execute the **Terraform** script for the bare infra deployment scenario. However, this script only handles installing and configuring components and does not create instances on the cloud providers.

## System Requirements

This section lists the recommended operating systems requirements, virtual machine instances requirements, and VPC requirements for implementing Chef Automate High Availability (HA) in your network infrastructure.

### Platform support

| Operating Systems                        | Tested                    |
| :--------------------------------------: | :-----------------------: |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8 (For 8 or above versions, **SELinux** configuration must be permissive. By default, in RHEL 8 **SELinux** configuration is enforced). Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 |packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x          |
| Centos (64 Bit OS)                       | 7                         |

### Virtual Machine (VM) Instances Type

| Instance          | Type         | RAM                                                   | Volume Size         | Volume Type | Volume iops |
| :---------------: | :----------: | :---------------------------------------------------: | :-----------------: | :---------: | :---------: |
| PostgreSQL        | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 150 |
| Elasticsearch     | m5.large     | 8 GB RAM for test and 16 GB for production. vCPU - 2. | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 300 |
| Chef Automate     | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 100 |
| Chef Infra Server | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). | |gp2 | | 100 |

{{< note >}}

ES volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans. The above table includes AWS instances types. However, for Bare-infra deployment or In-premises deployment types, you can choose the above requirements for VM like RAM.

For **Elasticsearch** and **PostgresSQL**, a minimum of three node clusters is required.

{{< /note >}}

## Chef Automate HA Components

### Automate-ha-cluster-ctl

Provides commands such as `automate-cluster-ctl provision/deploy`, which is installed via automate-backend-deployment.

### Automate-ha-ctl

Aids connect the backend (**postgres** and **elasticsearch**) databases using an automate configuration file and **Terraform** without any manual intervention.

### Automate-ha-curator

**Elasticsearch** curator aids in curating and managing the **Elasticsearch** indices and snapshots by obtaining the entire actionable list of indices (or snapshots) from the cluster. This component is the same as the default curator. It's written in a **hab** package to merge applications in a hab environment.

### Automate-ha-deployment

Aids in setting up a workspace for Chef Automate HA environment. For example, `/hab/a2_deploy_workspace`. It also includes **terraform** code, some necessary **scripts**, **inspecs**, **tests**, **Makefile** and so on.

### Automate-ha-elasticsearch

Includes the **elasticsearch** configuration and builds the **elasticsearch** package. It is installed in the backend nodes.

### Automate-ha-elasticsidecar

Provides a sidecar service for **automate-backend-elasticsearch** that reads user's credentials and passwords of the **elasticsearch** binding and applies it to **Elasticsearch** using the **odfe** tooling.

### Automate-ha-haproxy

Aids in sending a request to the leader node and is placed on **postgres** cluster.

### Automate-ha-kibana

Aids in viewing logs at a central place. The **Kibana Dashboard** displays the **postgres** and **elasticsearch** nodes with system metrics such as RAM and CPU details, and services logs such as **pgleaderchk**, **curator**.

### Automate-ha-journalbeat

Aids in collecting **journalctl** logs like all the service logs. It is placed on all **postgres** and **elasticsearch** nodes. It collects and sends the log information to **elasticsearch**, and the **Kibana Dashboard** displays the respective log information.

### Automate-ha-metricbeat

This component is placed on all **postgres** and **elasticsearch** nodes. It collects all related metrics and sends them to
**elasticsearch**. The **Kibana Dashboard** displays the respective log information.

### Automate-ha-pgleaderchk

This component is used in a proxy health check to determine where to route SQL requests. A **golang** service that checks the local **PostgreSQL** instance to view if it's a *Leader*.

### Automate-ha-postgresql

This component is a wrapper package of *core/postgresql11* for Chef Automate that provides a backend **HA PostgreSQL**.
