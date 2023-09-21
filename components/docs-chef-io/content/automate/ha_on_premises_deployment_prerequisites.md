+++
title = "On-Premises Prerequisites"

draft = false

automate = "On-Premises Prerequisites"
[menu]
  [menu.automate]
    title = "On-Premises Prerequisites"
    parent = "automate/deploy_high_availability/ha_prerequisites"
    identifier = "automate/deploy_high_availability/ha_prerequisites/ha-on-premises-deployment-prerequisites.md On-Premises Prerequisites"
    weight = 20
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

{{< warning >}}

The following  prerequisites are according to the standard Chef Automate HA setup. You can contact the customer success manager or account manager if you use any specified version not mentioned here or a third-party extension or software.

{{< /warning >}}

Before installing Chef Automate HA in on-premises deployment mode, ensure you have taken a quick tour of this prerequisite page.

## Chef Automate Architecture

Chef recommends using an 11 node cluster for a standard Chef Automate HA on-premises deployment, as detailed in the table below:

| Service Type      | Count |
|-------------------|-------|
| Chef Automate     | 2     |
| Chef Infra Server | 2     |
| PostgreSQL DB     | 3     |
| OpenSearch DB     | 3     |
| Bastion Machine   | 1     |

This topology requires two load balancers and two DNS entries with certificates. Refer to the [architectural page](/automate/ha/#chef-automate-ha-architecture/) for further guidance.

Chef Automate HA requires a [high availability Chef Infra Server](/server/install_server_ha/) deployment; it does not support a standalone Chef Infra Server deployment.

You can deploy a Chef Automate high availability cluster on AWS or Google Cloud Platform (GCP) VMs.

On-prem deployments of Chef Automate HA supports making backups on file system (FS) or object storage (S3/MinIO/Google Cloud Storage).

## Software Requirements

The software requirements for nodes in the cluster and for other external Chef and non-Chef tools are discussed below.

### Node Software Requirements

The operating system and the supported version for different nodes in the on-premises deployment of Automate HA are mentioned below:

| Operating Systems                        | Supported Version         |
| :--------------------------------------  | :-----------------------  |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x, 20.04.x |
| Centos (64 Bit OS)                       | 7                         |
| Amazon Linux 2 (64 Bit OS)               | 2 (kernel 5.10)           |
| SUSE Linux Enterprise Server             | 12.5                      |

### Minimum Supported Chef Tool Versions

Current Automate HA supports integration with the following Chef tools:

- Chef Infra Server version: 14.0.58+
- Chef Inspec version: 4.3.2+
- Chef Infra Client: 17.0.242+
- Chef Habitat: 0.81+

We do not support **Chef Manage** integration in the ongoing Automate version.

### External Supported Softwares

Current Automate HA integrates with the following non-Chef tools:

- **SQL Database:** PostgreSQL: 13.5
- **NoSQL Database:** OpenSearch: 1.3.7
- **Load Balancer:** NGINX: 1.21.3 or HA Proxy: 2.2.18 or AWS Application Load Balancer

## Hardware Requirements

{{< note >}}

- Refer to [Performance Benchmarks](/automate/ha_performance_benchmarks) for more details on the hardware requirements.
- Make sure the hardware requirement is not less than the recommended [Minimum Hardware Requirement](/automate/ha_on_premises_deployment_prerequisites/#minimum-hardware-requirement)
- Contact your network manager to set up the above pre-requisites.

{{< /note >}}

### Minimum Hardware Requirement

| Instance          | Count | vCPU | RAM | Storage Size(/hab) | AWS Machine Type | GCP Machine Type | Additional Space      |
| ----------------- | ----- | ---- | --- | ------------------ | ---------------- | ---------------- | -----------------     |
| Chef Automate     | 2     | 2    | 8   | 200 GB             | m5.large         | n2-standard-2    | /var/tmp=5% /root=20% |
| Chef Infra Server | 2     | 2    | 8   | 200 GB             | m5.large         | n2-standard-2    | /var/tmp=5% /root=20% |
| PostgreSQL DB     | 3     | 2    | 8   | 200 GB             | m5.large         | n2-standard-2    | /var/tmp=5% /root=20% |
| OpenSearch DB     | 3     | 2    | 8   | 200 GB             | m5.large         | n2-standard-2    | /var/tmp=5% /root=20% |
| Bastion Machine   | 1     | 2    | 8   | 200 GB             | m5.large         | n2-standard-2    | /var/tmp=5% /root=20% |

{{< note >}}
For production, OpenSearch volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans.
{{< /note >}}

### Load Balancer

Load Balancers in on-premises deployment need to be set up according to [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/).

You can set up your [load balancer](/automate/loadbalancer_configuration/) using:

- [NGINX 1.21.3](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
- [HA Proxy 2.2.18](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)
- [AWS Application Load Balancer](https://aws.amazon.com/elasticloadbalancing/application-load-balancer/)

## Firewall Checks

The Chef Automate HA cluster requires multiple ports for the frontend and backend servers to operate effectively.

The first column in the table below represents the source of the connection. The table's other columns represent the destination with the matrix value as a port number. The specified port numbers need to be opened on the origin and destination.

|               | Chef Automate  | Chef Infra Server | PostgreSQL                          | OpenSearch                           | Bastion | Automate Load Balancer |
|---------------|----------------|-------------------|-------------------------------------|--------------------------------------|---------| ------------- |
| Chef Automate |                |                   | 7432, 9631                                | 9200, 9631                                  |         |               |
| Infra Server  |                |                   | 7432, 9631                                 | 9200, 9631                                  |         | 443              |
| PostgreSQL    |                |                   | 9631, 7432, 5432, 6432, 9638<br/>UDP 9638 |                                      |         |               |
| OpenSearch    |                |                   |                                     | 9631, 9200, 9300, 9638 <br/>UDP 9638 |         |               |
| Bastion       | 22, 9631, 9638, 7799 | 22, 9631, 9638, 7799    | 22, 9631, 9638, 7432, 7799                | 22, 9631, 9638, 9200, 7799                 |         | 22            |
| Automate Load Balancer | 443, 80        | 443, 80           |                                     |                                      |         |               |

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
| TCP      | 7432        | HAProxy, which redirects to PostgreSQL Leader            |
| TCP      | 6432        | Re-elect PostgreSQL Leader if PostgreSQL leader is down  |
| TCP      | 5432        | Allows PostgreSQL nodes to connect with each other       |
| TCP/UDP  | 7799        | Allows bastion to connect with automate-verify service   |

## Certificates

Generate the certificates using recommended tools and supported algorithms and versions mentioned below:

- OpenSSL: 1.0.2zb-fips
- OpenSSL Algorithms: PBE-SHA1-3DES, RSA (2048), SHA-256
- Certificate Format: X509 V3(PEM format) ,Private key is in PKCS8 format


To understand how to generate certificates, refer to the [Certificate Generation](/automate/ha_cert_selfsign/#creating-a-certificate) documentation.


## Deployment Specific Pre-requisites

The on-premises deployment specific pre-requisites are as follows:

### Infra Server

- Chef Automate HA comes with a bundled Infra Server, and it is recommended not to use any external server in Automate HA. Using an external server will lose the Automate HA functionalities, and things may not work as expected.

### Access

- All Virtual Machines or Machines should be up and running.
- We need a local user `hab` and local group `hab` linked together to complete the deployment process successfully.
- If they are unavailable, the SSH user should have privileges to create local users and groups so that the deployment process can create the required local user `hab` and local group `hab`.
- Currently, we only support local Linux users and groups for Installation flow. We don't support AD or LDAP managed users in nodes.
- The SElinux config should either be disabled or permissive.

### Storage Space

- Operating System Root Volume (`/`) must be at least 40GB. Temporary space (`/var/tmp`) must be at least 10GB.
- Separate Hab volume should be provisioned and mounted at `/hab` with at least 200GB for all nodes except OpenSearch.
- For OpenSearch nodes, /hab volume should be calculated based on the data retention policy, and use the [Performance Benchmarks](/automate/ha_performance_benchmarks) for estimation.

### SSH User

- SSH users should use key-based SSH login without a passphrase.
- The user's SSH key should be generated using algorithms `ed25519` and `RSA(2048)` without a passphrase.
- This SSH user should be a local Linux user on all the machines.
- This SSH user should have sudo privileges on all the machines.
- SSH user should have write permission in nodes.
- The SSH user should access all machines using the same SSH private key.

### Cluster Setup

- LoadBalancers should be set up according to [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/).
- Network ports should be opened as per [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/) needs as explained in [Security and Firewall page](/automate/ha_on_premises_deployment_prerequisites/#firewall-checks).
- DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.
- Domain Certificates should be created and added for `chefautomate.example.com`, and `chefinfraserver.example.com` in the Load Balancers.
- We expect the customer to have all the Cluster related items ready before deployment. Customer experts will set up things like Load Balancer, Ports, and DNS with certificates.

### Config Changes

- [Config Patch](/automate/ha_config/#patch-configuration/) in the whole application will result in downtime. For example, if you change or update something in OpenSearch or PostgreSQL, they will restart, resulting in restarting everything.
- [Certificate Rotation](/automate/ha_cert_rotation/) will also change the system's configuration, leading to restarting the whole application.

To learn more about the above deployment, visit our [on-premises deployment](/automate/ha_onprim_deployment_procedure/) page.

## External Managed Databases

Set up the databases with password-based authentication.

### AWS Managed

- AWS RDS PostgreSQL: 13.5
- AWS OpenSearch: 1.3

Configure the backup only with **S3** when using AWS managed databases.

### Customer Managed

- PostgreSQL: 13.5
- OpenSearch: 1.3.7

## Upgrade

Things to keep in mind while upgrading are:

- Backend upgrades will restart the backend service, which takes time for the cluster to be healthy.
- The Upgrade command currently supports only minor upgrades.
- A downtime will occur while upgrading the **frontend**, **backend** or the **workspace**.
- Rolling upgrades are not supported.

## Disaster Recovery

Chef Automate HA supports disaster recovery in active/passive mode. The primary cluster will be in active mode, and the disaster recovery cluster will be in passive mode.

Active/Active Disaster Recovery is not supported right now as we do not support streaming of data across clusters and automatic fail-over switching of clusters.

The requirements for disaster recovery setup (Active/Passive) are:

- Two identical clusters located in different data centers or cloud provider regions.
- Network Attached Storage (NAS) or Object Store (S3/MinIO/Google cloud storage) should be available in both data centers/regions.
- Set up scheduled jobs to run backup and restore commands on both clusters. We recommend using **cron** to schedule the jobs.

To know more about the on-premises deployment disaster recovery, visit our [Disaster Recovery Setup](/automate/ha_disaster_recovery_setup/) page.

## Migration

### Common Notes

- Migrations involve downtime depending on how much data you have and the type of setup you are running.

- Migration cannot be done from more than 1 Standalone Automate, more than 1 Standalone Infra Server, or more than 1 Chef Backend to a Single Automate HA cluster.

- Automate HA will always have Chef Automate and Chef Infra Server running in the cluster.

| Existing System | Supported Setup Type | Minimum Eligible System Version | Maximum Eligible System Version |  Pre-requisite Before Migration |
|-----------------|----------------------|---------------------------------|-----------|------------------------------|
| Chef Automate | [Standalone](/automate/install/) | Automate 2020XXXXXX |    | To migrate to the Managed OpenSearch Automate HA cluster, the current standalone Chef Automate version should be at most 4.3.0. |
| Chef Backend | [Chef Backend Cluster](/server/install_server_ha/) | Backend 2.X and Infra Server 14.X | Chef Infra Server 15.4.0 | Chef Backend using PostgreSQL storage for Cookbooks should only migrate to Automate HA. | 
| Chef Infra Server | [Standalone](/server/install_server/#standalone)<br />[Tiered](/server/install_server_tiered/) | Infra server 14.XXX | Chef Infra Server 15.4.0 | Chef Infra Server using PostgreSQL storage for Cookbooks should only migrate to Automate HA. |
| A2HA | PS Lead A2HA On-Premises Deployment |Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | The A2HA cluster-mounted backup file system should also be attached to Automate HA cluster.<br />In case of In-Place migration, the volume having `/hab` should have more than 60% free space on each node. |

{{< note >}}

- Suppose you have done any modification to the standard installation setup mentioned above. In that case, we do not support migration to Automate HA.
- We don't recommend in-place migration of A2HA and Chef Backend to Automate HA as the system level changes like ports, system users, and groups may conflict with the successful installation of Automate HA. Also, no easy rollback process is available. This will lead to higher downtime or loss of existing setup.

{{< /note >}}

## Backup and Restore

In on-premises deployment of Automate HA, we support [**Network File System (NFS)**](/automate/ha_backup_restore_file_system/) or [**Object Storage (S3/MinIO/Google Cloud Storage)**](/automate/ha_backup_restore_object_storage/) for taking backup.

Encrypted S3 buckets are only supported with Amazon S3 managed keys (SSE-S3).
