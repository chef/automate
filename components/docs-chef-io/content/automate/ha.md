+++
title = "Deployment Workflows"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Deployment Workflows"
    parent = "automate/install"
    identifier = "automate/install/ha.md Deployment Workflows"
    weight = 230
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
   1. Modify **VPC** and **subnet** values as required.
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

This section lists the Chef Automate High Availability (HA) components and their purpose.

## HA Components
## Automate-ha-cluster-ctl

Provides commands such as `automate-cluster-ctl provision/deploy`, which is installed via automate-backend-deployment.

## Automate-ha-ctl

Aids connect the backend (**postgres** and **elasticsearch**) databases using an automate configuration file and **Terraform** without any manual intervention.

## Automate-ha-curator

**Elasticsearch** curator aids in curating and managing the **Elasticsearch** indices and snapshots by obtaining the entire actionable list of indices (or snapshots) from the cluster. This component is the same as the default curator. It's written in a **hab** package to merge applications in a hab environment.

## Automate-ha-deployment

Aids in setting up a workspace for Chef Automate HA environment. For example, `/hab/a2_deploy_workspace`. It also includes **terraform** code, some necessary **scripts**, **inspecs**, **tests**, **Makefile** and so on.

## Automate-ha-elasticsearch

Includes the **elasticsearch** configuration and builds the **elasticsearch** package. It is installed in the backend nodes.

## Automate-ha-elasticsidecar

Provides a sidecar service for **automate-backend-elasticsearch** that reads user's credentials and passwords of the **elasticsearch** binding and applies it to **Elasticsearch** using the **odfe** tooling.

## Automate-ha-haproxy

Aids in sending a request to the leader node and is placed on **postgres** cluster.

## Automate-ha-kibana

Aids in viewing logs at a central place. The **Kibana Dashboard** displays the **postgres** and **elasticsearch** nodes with system metrics such as RAM and CPU details, and services logs such as **pgleaderchk**, **curator**.

## Automate-ha-journalbeat

Aids in collecting **journalctl** logs like all the service logs. It is placed on all **postgres** and **elasticsearch** nodes. It collects and sends the log information to **elasticsearch**, and the **Kibana Dashboard** displays the respective log information.

## Automate-ha-metricbeat

This component is placed on all **postgres** and **elasticsearch** nodes. It collects all related metrics and sends them to
**elasticsearch**. The **Kibana Dashboard** displays the respective log information.

## Automate-ha-pgleaderchk

This component is used in a proxy health check to determine where to route SQL requests. A **golang** service that checks the local **PostgreSQL** instance to view if it's a *Leader*.

## Automate-ha-postgresql

This component is a wrapper package of *core/postgresql11* for Chef Automate that provides a backend **HA PostgreSQL**.

Chef Automate High Availability (HA) operating systems requirements, virtual machine instances requirements, and VPC requirements for bare metal and AWS deployments.

## Architecture Reference

Chef Automate HA uses a **Leader-Follower** strategy. The following architecture diagram shows the Chef Automate HA cluster deployed with Chef Automate, Chef Infra Server, Postgres, and Elasticsearch.

![High Availability Architecture](/images/automate/ha_architecture.png)

PostgreSQL stores application service, secret, recovery, data.
Elasticsearch stores the data generated by Chef Infra Client runs and InSpec compliance scans and makes them available in to Chef Automate. The load balancer distributes the data to each of the Chef Automate HA components.

**Journalbeat** and **Metricbeat** are common for all database instances. **Journalbeat** installed as an agent on the servers collects all the services logs and forwards them to Elasticsearch. **Metricbeat** installed on the servers periodically collects metrics from the operating system and services running on the server and sends them to the **Kibana**.

**Kibana** is an open-source, web-based data visualization and analytical tool that allows you to explore, visualize, and build a dashboard over the log data massed in Elasticsearch clusters. It is a part of the Elastic Stack and integrates with Elasticsearch. The **Kibana** **Dashboard** is a collection of charts, graphs, metrics, searches, and maps in a single pane and provides at-a-glance insights into data from multiple perspectives enabling you to drill down into the details.

### Chef Automate Clusters

The Chef Automate HA architecture involves two different clusters part of the main cluster, which are:

- Chef Automate Backend Cluster and Nodes

The backend components connect into the frontend Chef Habitat supervisor cluster. In the Chef Habitat supervisor, **postgres** and **elasticsearch** instances run. A minimum of three nodes is required for **Postgres** and **Elasticsearch** databases, where one becomes a leader, and others are followers.

- Chef Automate Frontend Cluster and Nodes

Chef Automate and Chef Infra Server act as frontend nodes and serve as a web UI with load balancer configurations.

### Chef Automate Cluster Nodes

The backend and frontend clusters comprise **four** different servers with HA mode, which are as follows:

1. Chef Automate
2. Chef Infra Server
3. Elasticsearch: An open-source search and analytics engine based on Apache Lucene and built with Java. It is a time-series and NoSQL database that stores data in an unstructured way and is used for indexing purposes.
4. PostgreSQL: An open-source relational database management system (RDBMS) emphasizing extensibility and SQL compliance.

<!-- ! -- These four components reside in a VPC under one network in AWS. Every node sits on a specific machine irrespective of a database. Single database for all three nodes of automate. -->

{{ note >}}

Elasticsearch internally manages the communication and backup, and does not follow any leader-follower strategy.

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
   1. Modify **VPC** and **subnet** values as required.
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

## HA System Requirements

This section lists the recommended operating systems requirements, virtual machine instances requirements, and VPC requirements for implementing Chef Automate High Availability (HA) for your network infrastructure or systems or applications or services.

## Platform support

| Operating Systems                        | Tested                    |
| :--------------------------------------: | :-----------------------: |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8 (For 8 or above versions, **SELinux** configuration must be permissive. By default, in RHEL 8 **SELinux** configuration is enforced). Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 |packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x          |
| Centos (64 Bit OS)                       | 7                         |

## Virtual Machine (VM) Instances Type

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

## HA Bastion

A [Bastion Host](https://en.wikipedia.org/wiki/Bastion_host#:~:text=A%20bastion%20host%20is%20a,the%20threat%20to%20the%20computer.) is a special-purpose computer or server on a network specifically designed and configured to withstand attacks. This serve type generally hosts a single application or process, for example, a proxy server or load balancer. All other services are limited to reduce the threat to the computer.

Its purpose is to provide access to a private network from an external network, such as the Internet or outside of a firewall and involves access from untrusted networks or computers. These computers are also equipped with special networking interfaces to withstand high-bandwidth attacks through the internet.

Bastion servers are instances that reside within your public subnet and are accessed using SSH. The purpose of a bastion host is to restrict access to a private network from an external network. Once remote connectivity establishes with the bastion host, it allows you to use SSH to log in to other instances (within private subnets) deeper within your network.

The bastion hosts provide secure access to Linux instances located in the private and public subnets.

## Bastion Host for Chef Automate High Availability (HA)

The Virtual machine is required for either of the Chef Automate HA deployment types to trigger the deployment, which is actually a bastion host. This section explains the bastion host requirements and configurations for the two deployment modes of the Chef Automate HA.

### Bastion Host Requirements for On-Premise Deployment

- Bastion Server/host IP address
- Instance type: 2 vCPU
- Operating System: Ubuntu 20.04
- Memory: Minimum of 4GB
- Hard Disk Space - 100 GB
- Ports to be publicly accessible: 22 and 9631

### Bastion Host Requirements for AWS (Amazon Web Services)

- [AWS Credential configured on your bastion host](( {{< relref "ha_configure_aws_credentials.md" >}} )).
- Create the certificate for the DNS
- Operating System (OS): Bastion host with Ubuntu 20.04 or centOs-7 or RHEL-7
- AWS instance type: *t2.medium*
- Memory: Minimum of 4GB
- Hard Disk Space - 100 GB
- SSH: VPC to Port 22, publicly accessible
- [Setup Virtual Private Cloud (VPC) in AWS](( {{< relref "ha_vpc_setup.md" >}}))

## HA Auto Install

Both deployment models require you to install and configure Chef Automate on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

## Download and Install the Chef Automate Utility

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.
- Type the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**. The command downloads the Chef Automate utility package in .zip format, and installs the utility by providing the execute permission to the Chef Automate file.

  The installation of the Chef Automate utility completes, and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_aws_chef_automate_install.png" alt="Chef Automate Utility Installation">}}

## HA Deploy AWS

This page explains how to deploy Chef Automate High Availability (HA) in your network premises/ infrastructure using Amazon Web Services (AWS). To deploy Chef Automate HA, execute the following steps in the listed order:

1. Set up the [Prerequisites for Chef Automate HA Deployment](( {{< relref "ha_system_requirements.md" >}} )).
1. Obtain an AWS account or if you already have one, sign on to your AWS account.
1. Setup the [Bastion Host AWS requirements](( {{< relref "ha_bastion.md#Bastion Host Requirements for AWS (Amazon Web Services)" >}} )).
1. Configure [Bastion Host for AWS ](( {{< relref "ha_aws_bastion.md#Bastion Host Configuration" >}} )).
1. Ensure you have [Chef Automate utility](( {{< relref "ha_auto_install.md" >}})) installed, else download and install the latest version.
1. Connect your [Bastion host to your AWS instance](( {{< relref "ha_aws_ssh_connect_bastion.md" >}} )).
1. Create an [AWS Identity and Access Management IAM user](( {{< relref "ha_iam_user.md" >}} )).
1. Create the certificate for the Chef Automate and Chef Server load balancers.
1. Create the certificates for security and authentication purposes. _optional_
1. Rotate the certificates if the certificates are expired or compromised. _optional_
1. Enable *dnshostname* in VPC, which determines whether the VPC supports assigning public DNS hostnames to instances with public IP addresses.
1. Execute [AWS Deployment commands](( {{< relref "ha_aws_deploy_steps.md" >}} )) to provision the Chef Automate HA on your cloud  network infrastructure.

{{< note >}}

A DNS hostname uniquely names a computer and consists of a host name and a domain name. DNS servers resolve DNS hostnames to their
corresponding IP addresses. To set up DNS in your VPC, ensure that DNS hostnames and DNS resolution are both enabled in your VPC.

Refer [Setting up DNS in Your VPC](https://docs.aws.amazon.com/glue/latest/dg/set-up-vpc-dns.html) page.

If the DNS attributes, *enableDnsSupport* and *enableDnsHostnames* are true, instances in the VPC is set with public DNS hostnames.
The default for these attributes are `false` when the VPC is a default VPC or the VPC is created using the VPC console wizard.

{{ < /note >}}

1. Execute the [Chef Automate HA deployment commands](( {{< relref "ha_aws_deploy_steps.md" >}} )).

## AWS Infrastructure Resources

The Chef Automate HA deployment using AWS creates the following network resources:

- Three instances for ElasticSearch node.

- Three instances for PostgreSQL node.

- One instance for Chef Automate server. However, based on your requirements, we can add more instances for the Chef Automate server.

- One instance for Chef Infra Server each. However, based on your requirements, we can add more instances for the Chef Infra server.

{{< figure src="/images/automate/ha_aws_resources1.png" alt="Chef Automate HA ElasticSearch and PostgreSQL Instances">}}

- Two load balancers and two respective target groups. One each for the Chef Automate server and Chef Infra server.

{{< figure src="/images/automate/ha_aws_resources2.png" alt="Chef Automate HA Load Balancers and Target Groups">}}

- Elastic File System (EFS) for backup of all the instances.

{{< figure src="/images/automate/ha_aws_resources3.png" alt="Chef Automate HA EFS Backup">}}

- Three private subnets (with no internet access) and three public subnets (with internet access).

{{< figure src="/images/automate/ha_aws_resources4.png" alt="Chef Automate HA Public and Private Subnets">}}

- One route table.

{{< figure src="/images/automate/ha_aws_resources5.png" alt="Chef Automate HA Route Table">}}

- One Elastic IP address.

{{< figure src="/images/automate/ha_aws_resources6.png" alt="Chef Automate HA Elastic IP">}}

## Clear AWS Deployment Infrastructure

Follow these steps to delete the *Terraform* and *HA Deployment Infrastructure*:

1. Navigate to the Chef Automate workspace folder where we have deployed HA. For example, `cd /hab/a2_deploy_workspace/terraform`.

1. Type the command, `terraform destroy` and press **Enter**. This command removes the terraform deployed at your workspace and all the instances.

1. Type the command, `rm -rf /hab/a2_deploy_workspace` and press **Enter**. This command removes the workspace.

