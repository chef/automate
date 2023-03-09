+++
title = "On-Premise Prerequisites"

draft = false

gh_repo = "On-Premise Prerequisites"
[menu]
  [menu.automate]
    title = "On-Premise Prerequisites"
    parent = "automate/deploy_high_availability"
    identifier = "automate/settings/ha-on-premise-deployment-prerequisites.md On-Premise Prerequisites"
    weight = 12
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}
The below prerequisites are according to the standard Chef Automate HA setup. You can contact the customer success manager or account manager if you use any specified version not mentioned here or a third-party extension or software.
{{< /warning >}}

Before installing Chef automate HA in On-premise deployment mode, ensure you have taken a quick tour of this prerequisite page.

## Chef Automate Architecture

We recommend using 11 node cluster for standard Automate HA on-premise deployment, as detailed in the table below:

| Service Type      | Count |
|-------------------|-------|
| Chef Automate     | 2     |
| Chef Infra Server | 2     |
| Postgresql DB     | 3     |
| Opensearch DB     | 3     |
| Bastion Machine   | 1     |

Additionally, this topology requires two load balancers and 2 DNS entries with certificates. Refer to the [architectural page](/automate/ha/#chef-automate-ha-architecture/) for further guidance.

We recommend using Chef Infra Server managed by Automate HA to have high availability for both Automate and Infra Server. External Standalone Infra Server will violate this high availability requirement.

## Software Requirements

The software requirements of the nodes in the cluster and other external Chef and non Chef tools are discussed below:

### Node Software Requirements

The operating system and the supported version for different nodes in the on-premise deployment of Automate HA are mentioned below:

| Operating Systems                        | Supported Version         |
| :--------------------------------------  | :-----------------------  |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x, 20.04.x |
| Centos (64 Bit OS)                       | 7                         |
| Amazon Linux 2 (64 Bit OS)               | 2 (kernel 5.10)           |
| SUSE Linux Enterprise Server 12 SP5      | 12                        |

### Minimum Supported Chef Tool Versions

Current Automate HA supports integration with the following Chef tools:

- Chef Infra Server version: 14.0.58+
- Chef Inspec version: 4.3.2+
- Chef Infra Client: 17.0.242+
- Chef Habitat: 0.81+

We do not support **Chef Manage** and **Supermarket** integration in the ongoing Automate version.

### External Supported Softwares

Current Automate HA works with the following non-Chef tools:

- PostgreSQL: 13.5
- OpenSearch: 1.3.7
- NGINX: 1.21.3
- HA Proxy: 2.2.18

## Hardware Requirements

{{< note >}} Use a [Hardware Calculator](/calculator/automate_ha_hardware_calculator.xlsx) to check how much hardware you will need for your use case. {{< /note >}}

We have some sample values based on the performance benchmarking tests to give you an apt hardware configuration. Refer to the table below to populate things in the **Hardware Calculator** according to your requirement. The table below shows the tested **assumptions** and has no exact value.

You can use the below assumptions in the calculator to drive into your hardware requirement:

| Assumption                            | Value | Unit     |
|---------------------------------------|-------|----------|
| Number of Nodes sending data          | 5000  |          |
| Frequency of Compliance Scan          | 1     | Per Hour |
| Frequency of Client runs (Infra runs) | 1     | Per Hour |
| Frequency of Event Feed               | 1     | Per Hour |
| Data Retention policy                 | 1     | Days     |
| Compliance scan report size           | 400   | KB       |
| Client Run (Infra run) size           | 300   | KB       |
| Event Feed update size                | 2     | KB       |
| No. of Shards in OpenSearch Index     | 2     |

The machine requirements based on the above assumptions are listed below:

| Instance          | Count | vCPU | RAM | Storage Size(/hab) | AWS Machine Type | Additional Space |
|-------------------|-------|------|-----|------------------- |------------------|------------------|
| Chef Automate     | 2     | 2    | 8   | 80 GB              | m5.large         |/tmp=5%  /root=20%|
| Chef Infra Server | 2     | 2    | 8   | 80 GB              | m5.large         |/tmp=5%  /root=20%|
| Postgresql DB     | 3     | 2    | 8   | 150 GB             | m5.large         |/tmp=5%  /root=20%|
| Opensearch DB     | 3     | 2    | 8   | 58.9 GB            | m5.large         |/tmp=5%  /root=20%|
| Bastion Machine   | 1     | 2    | 8   | 150 GB             | m5.large         |/tmp=5%  /root=20%|

{{< note >}}
For production, OpenSearch volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans.
{{< /note >}}

### Load Balancer

Load Balancers in on-premise deployment are set up according to [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/).

You can set up your [load balancer](/automate/loadbalancer_configuration/) using:

- [NGINX 1.21.3](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
- [HA Proxy 2.2.18](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)
- [AWS Application Load Balancer](https://aws.amazon.com/elasticloadbalancing/application-load-balancer/)

## Firewall Checks

The Chef Automate High Availability (HA) cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. Below is a breakdown of those ports and what needs to be opened for each set of servers.

**Ports required for all Machines**

The first column in the table below represents the source of the connection. The table's other columns represent the destination with the matrix value as a port number. The specified port numbers need to be opened on the origin and destination.

|               | Chef Automate  | Chef Infra Server | Postgresql                          | OpenSearch                           | Bastion | Load Balancer |
|---------------|----------------|-------------------|-------------------------------------|--------------------------------------|---------| ------------- |
| Chef Automate |                |                   | 7432                                | 9200                                 |         |               |
| Infra Server  | 443            |                   | 7432                                | 9200                                 |         |               |
| PostgreSQL    |                |                   | 9631, 7432, 5432, 6432, 9638<br/>UDP 9638 |                                      |         |               |
| OpenSearch    |                |                   |                                     | 9631, 9200, 9300, 9638 <br/>UDP 9638 |         |               |
| Bastion       | 22, 9631, 9638 | 22, 9631, 9638    | 22, 9631, 9638, 7432                | 22, 9631, 9638, 9200                 |         | 22            |
| Load Balancer | 443, 80        | 443, 80           |                                     |                                      |         |               |

{{< note >}} Custom SSH port is supported, but use the same port across all the machines. {{< /note >}}

**Port usage definitions**

| Protocol | Port Number | Usage                                                    |
|----------|-------------|----------------------------------------------------------|
| TCP      | 22          | SSH to configure services                                |
| TCP      | 9631        | Habitat HTTP API                                         |
| TCP      | 443         | Allow Users to reach UI / API                            |
| TCP      | 80          | Optional, Allows users to redirect to 443                |
| TCP      | 9200        | OpenSearch API HTTPS Access                              |
| TCP      | 9300        | Allows OpenSearch node to distribute data in its cluster |
| TCP/UDP  | 9638        | Habitat gossip (UDP)                                     |
| TCP      | 7432        | HAProxy, which redirects to Postgresql Leader            |
| TCP      | 6432        | Re-elect Postgresql Leader if Postgresql leader is down  |

## Certificates

Generate the certificates using recommended tools and supported algorithms and versions mentioned below:

- OpenSSL: 1.0.2zb-fips
- OpenSSL Algorithms: PBE-SHA1-3DES, RSA (2048), SHA-256
- Certificate Format: PKCS 8

To understand how to generate certificates, refer to the [Certificate Generation](/automate/ha_cert_selfsign/#creating-a-certificate) documentation.

## Deployment Specific Pre-requisites

The on-premise deployment specific pre-requisites are as follows:

### Infra Server

- Chef Automate HA comes with bundled Infra Server, and it is recommended not to use any external server in Automate HA. Using an external server will lose the Automate HA functionalities, and things may not work as expected.

### Access

- All Virtual Machines or Machines should be up and running.
- We need a local user `hab` and local group `hab` linked together to complete the deployment process successfully.
- If they are unavailable, the SSH user should have privileges to create local users and groups so that the deployment process can create the required local user `hab` and local group `hab`.
<!-- We currently don't support AD managed users in nodes. We only support local Linux users. -->
- The SElinux config should either be disabled or permissive.

### Storage Space

- Operating System Root Volume (`/`) must be at least 40GB. Temporary space (`/var/tmp`) must be at least 5GB.
- Separate Hab volume should be provisioned and mounted at `/hab` with at least 100GB free space for all nodes except OpenSearch.
- For OpenSearch nodes, /hab volume should be calculated based on the data retention policy, and use the  [hardware calculator](/calculator/automate_ha_hardware_calculator.xlsx) for estimation.

### SSH User

- SSH users should use key-based SSH login without a passphrase.
- The user's SSH key should be generated using algorithms `ed25519` and `RSA(2048)` without a passphrase.
- This SSH user should be a local Linux user on all the machines.
- This SSH user should have sudo privileges on all the machines.
- The SSH user should access all machines using the same SSH private key.

### Cluster Setup

- LoadBalancers should be set up according to [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/).
- Network ports should be opened as per [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/).
- DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.
- Domain Certificates should be created and added for `chefautomate.example.com`, and `chefinfraserver.example.com` in the Load Balancers.

### Config Changes

- [Config Patch](/automate/ha_config/#patch-configuration/) in the whole application might result in downtime. For example, if you change or update something in OpenSearch or PostgreSQL, they will restart, resulting in restarting everything.
- [Certificate Rotation](/automate/ha_cert_rotation/) will also change the system's configuration, leading to restarting the whole application.

To learn more about the above deployment, visit our [on-premise deployment](/automate/ha_onprim_deployment_procedure/) page.

## External Managed Databases

Set up the databases with password-based authentication.

### AWS Managed

- AWS RDS PostgreSQL: 13.5
- AWS OpenSearch: 1.3

### Customer Managed

- PostgreSQL: 13.5
- OpenSearch: 1.3.7

## Upgrade

Things to keep in mind while upgrading are:

- Backend upgrades will restart the backend service, which takes time for the cluster to be healthy.
- Upgrade command currently supports only minor upgrades.
- A downtime might occur while upgrading the **frontend**, **backend** or the **workspace**.
- Currently, rolling upgrades are not supported.

## Disaster Recovery

Chef Automate HA supports disaster recovery in active/passive mode. The primary cluster will be in active mode, and the disaster recovery cluster will be in passive mode.

Active/Active Disaster Recovery is not supported right now as we do not support streaming of data across clusters and automatic failover switching of clusters.

The requirements for disaster recovery setup (Active/Passive) are:

- Two identical clusters located in different data centers or cloud provider regions.
- Network Attached Storage (NAS) or Object Store (S3) should be available in both data centers/regions.
- Set up scheduled jobs to run backup and restore commands on both clusters. We recommend using **cron** to schedule the jobs.

To know more about the on-premise deployment disaster recovery, visit our [Disaster Recovery Setup](/automate/ha_disaster_recovery_setup/) page.

## Migration

### Common Notes

Migrations involve downtime depending on how much data you have and the type of setup you are running.

| Existing System | Minimum Eligible System Version | Maximum Eligible System Version |  Pre-requisite Before Migration | Notes | Not Supported Use Cases |
|-----------------|---------------------------------|-----------|------------------------------| ----- | ----------------------- |
| Chef Automate | Automate 2020XXXXXX |    |   | To migrate to Managed OpenSearch Automate HA cluster, the current standalone Chef Automate version should be at most 4.3.0. | Migration of 2 or more Chef Infra Servers to a single Automate HA is not supported. <br /> Migration of 2 or more Chef Infra Servers to a single Automate HA is not supported. |
| Chef Backend | Backend 2.X and Infra Server 14.X | Chef Infra Server 15.4.0 |    | Irrespective of whether you use Chef Automate, Chef Automate will run in Automate HA cluster. |  Chef Manage, or Private Chef Supermarket with Chef Backend should not migrate to Automate HA. <br /> Migration of 2 or more Chef Backends to a single Automate HA is not supported. |
| Chef Infra Server | Infra server 14.XXX | Chef Infra Server 15.4.0 |    | Irrespective of whether you use Chef Automate, Chef Automate will run in Automate HA cluster. |  Chef Manage, or Private Chef Supermarket with Chef Backend should not migrate to Automate HA. Automate HA does not support supermarket authentication with chef-server user credentials. <br /> Migration of 2 or more Chef Infra Server to a single Automate HA is not supported. |
| A2HA | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | The A2HA cluster-mounted backup file system should also be attached to Automate HA cluster. |    |    |
| In-Place A2HA | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | A2HA should be configured to take backup on a mounted network drive. <br />The volume having `/hab` should have more than 60% free space on each node. |    |    |

## Backup and Restore

In On-premise deployment, we support **Network File System(NFS)** or **Object Storage(S3/MinIO)** for taking [backup](/automate/ha_backup_restore_file_system/).
