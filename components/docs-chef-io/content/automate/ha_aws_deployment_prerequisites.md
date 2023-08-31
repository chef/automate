+++
title = "AWS Deployment Prerequisites"
draft = false
automate = "AWS Deployment Prerequisites"
[menu]
  [menu.automate]
    title = "AWS Deployment Prerequisites"
    parent = "automate/deploy_high_availability/ha_prerequisites"
    identifier = "automate/deploy_high_availability/ha_prerequisites/ha-aws-deployment-prerequisites.md AWS Deployment Prerequisites"
    weight = 30
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}
The below prerequisites are according to the standard Chef Automate HA setup. You can contact the customer success manager or account manager if you use any specified version not mentioned here or a third-party extension or software.
{{< /warning >}}

Before installing Chef automate HA in AWS deployment mode, ensure you have taken a quick tour of this prerequisite page.

## Chef Automate Architecture

We recommend using 11 node cluster for standard Automate HA AWS deployment, as detailed in the table below:

| Service Type      | Count |
| ----------------- | ----- |
| Chef Automate     | 1     |
| Chef Infra Server | 1     |
| PostgreSQL DB     | 3     |
| OpenSearch DB     | 3     |
| Bastion Machine   | 1     |

Additionally, this topology requires two load balancers and 2 DNS entries with certificates. Refer to the [architectural page](/automate/ha/#chef-automate-ha-architecture/) for further guidance.

We recommend using Chef Infra Server managed by Automate HA to have high availability for both Automate and Infra Server. External Standalone Infra Server will violate this high availability requirement.

## Software Requirements

The software requirements of the nodes in the cluster and other external Chef and non Chef tools are discussed below:

### Node Software Requirements

The operating system and the supported version for different nodes in AWS deployment of Automate HA are mentioned below:

| Operating Systems                    | Supported Version                                                                                                                                                                                                                                         |
| :----------------------------------- | :-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Red Hat Enterprise Linux (64 Bit OS) | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                   | 16.04.x, 18.04.x, 20.04.x                                                                                                                                                                                                                                 |
| Centos (64 Bit OS)                   | 7                                                                                                                                                                                                                                                         |
| Amazon Linux 2 (64 Bit OS)           | 2 (kernel 5.10)                                                                                                                                                                                                                                           |
| SUSE Linux Enterprise Server         | 12.5                                                                                                                                                                                                                                                      |

### Minimum Supported Chef Tool Versions

Current Automate HA supports integration with the following Chef tools:

-   Chef Infra Server version: 14.0.58+
-   Chef Inspec version: 4.3.2+
-   Chef Infra Client: 17.0.242+
-   Chef Habitat: 0.81+

We do not support **Chef Manage** and **Supermarket** integration in the ongoing Automate version.

### External Supported Softwares

Current Automate HA integrates with the following non-Chef tools:

**In AWS Deployment**

-   **SQL Database:** External not supported
-   **NoSQL Database:** External not supported
-   **Load Balancer:** External not supported

**In AWS Managed Services**

-   **SQL Database:** AWS RDS PostgreSQL: 13.5
-   **NoSQL Database:** AWS OpenSearch: 1.3
-   **Load Balancer:** External not supported

## Hardware Requirements

{{< note >}}

- Refer to [Performance Benchmarks](/automate/ha_performance_benchmarks) for more details on the hardware requirements.
- Make sure the hardware requirement in not lesser than the recommended [Minimum Hardware Requirement](/automate/ha_aws_deployment_prerequisites/#minimum-hardware-requirement)

{{< /note >}}

### Minimum Hardware Requirement

| Instance          | Count | vCPU | RAM | Storage Size(/hab) | AWS Machine Type | Additional Space  |
| ----------------- | ----- | ---- | --- | ------------------ | ---------------- | ----------------- |
| Chef Automate     | 1     | 2    | 8   | 200 GB             | m5.large         | /var/tmp=5% /root=20% |
| Chef Infra Server | 1     | 2    | 8   | 200 GB             | m5.large         | /var/tmp=5% /root=20% |
| PostgreSQL DB     | 3     | 2    | 8   | 200 GB             | m5.large         | /var/tmp=5% /root=20% |
| OpenSearch DB     | 3     | 2    | 8   | 200 GB             | m5.large         | /var/tmp=5% /root=20% |
| Bastion Machine   | 1     | 2    | 8   | 200 GB             | m5.large         | /var/tmp=5% /root=20% |

{{< note >}} For production, OpenSearch volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans. {{< /note >}}

### Load Balancer

LoadBalancers in AWS deployment are set up according to [Chef Automate HA Architecture](/automate/ha/). [AWS Application Load Balancer](https://aws.amazon.com/elasticloadbalancing/application-load-balancer/) will be setup during deployment automatically.

## Firewall Checks

The Chef Automate HA cluster requires multiple ports for the frontend and backend servers to operate effectively.

**Ports for Bastion before deployment**

| Port No. | Outgoing to | Incoming from |
| -------- | ----------- | ------------- |
| 22       | Subnet      | All           |
| 80       |             | Internet      |
| 443      |             | Internet      |

**Port mappings required before deployment:**

The first column in the table below represents the source of the connection. The table's other columns represent the destination with the matrix value as a port number. The specified port numbers need to be opened on the origin and destination.

|                 | Chef Automate        | Chef Infra Server    | PostgreSQL                                | OpenSearch                           | Bastion | Automate Load Balancer |
| --------------- | -------------------- | -------------------- | ----------------------------------------- | ------------------------------------ | ------- | ------------- |
| Chef Automate   |                      |                      | 7432, 9631                                       | 9200, 9631                                  |         |               |
| Infra Server    |                      |                      | 7432, 9631                                       | 9200, 9631                                  |         | 443              |
| PostgreSQL      |                      |                      | 9631, 7432, 5432, 6432, 9638<br/>UDP 9638 |                                      |         |               |
| OpenSearch      |                      |                      |                                           | 9631, 9200, 9300, 9638 <br/>UDP 9638 |         |               |
| Bastion         | 22, 9631, 9638, 7799 | 22, 9631, 9638, 7799 | 22, 9631, 9638, 7432, 7799                | 22, 9631, 9638, 9200, 7799           |         | 22            |
| Automate Load Balancer   | 443, 80              | 443, 80              |                                           |                                      |         |               |
| Internet Access |                      |                      |                                           |                                      | 80, 443 |               |

{{< note >}} Custom SSH port is supported, but use the same port across all the machines. {{< /note >}}

**Port usage definitions**

| Protocol | Port Number | Usage                                                     |
| -------- | ----------- | --------------------------------------------------------- |
| TCP      | 22          | SSH to configure services                                 |
| TCP      | 9631        | Habitat HTTP API                                          |
| TCP      | 443         | Allow Users to reach UI / API                             |
| TCP      | 80          | Optional, Allows users to redirect to 443                 |
| TCP      | 9200        | OpenSearch API HTTPS Access                               |
| TCP      | 9300        | Allows OpenSearch node to distribute data in its cluster. |
| TCP/UDP  | 9638        | Habitat gossip (UDP)                                      |
| TCP      | 7432        | HAProxy, which redirects to PostgreSQL Leader             |
| TCP      | 6432        | Re-elect PostgreSQL Leader if PostgreSQL leader is down   |
| TCP      | 5432        | Allows PostgreSQL nodes to connect with each other        |
| TCP/UDP  | 7799        | Allows bastion to connect with automate-verify service    |

## Certificates

Generate the certificates using recommended tools and supported algorithms and versions mentioned below:

-   OpenSSL: 1.0.2zb-fips
-   OpenSSL Algorithms: PBE-SHA1-3DES, RSA (2048), SHA-256
-   Certificate Format: X509 V3(PEM format) ,Private key is in PKCS8 format

To understand how to generate certificates, refer to the [Certificate Generation](/automate/ha_cert_selfsign/#creating-a-certificate) documentation.

## Deployment Specific Pre-requisites

The AWS deployment specific pre-requisites are as follows:

### AWS Cloud

-   Create an AWS Virtual Private Cloud (VPC) with an internet gateway before you start. For further knowledge, refer to the [VPC and CIDR creation](/automate/ha_vpc_setup/) page.
-   If you want to use Default VPC, create public and private subnets. If the subnet is not available. Please refer [to this](https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html)
-   Three private and three public subnets in a VPC (1 subnet for each AZ) are needed. As of now, only dedicated subnets for each AZ are supported.
-   It is recommended to create a new VPC.
-   Bastion must be in the same VPC for deployment.
-   **In AWS Managed Services:**
    -   Setup [AWS RDS PostgreSQL](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_GettingStarted.CreatingConnecting.PostgreSQL.html) 13.5 in the same VPC.
    -   Setup [AWS OpenSearch](https://docs.aws.amazon.com/opensearch-service/latest/developerguide/createupdatedomains.html) of version 1.3 in the same VPC.

### Infra Server

-   Chef Automate HA comes with bundled Infra Server, and it is recommended not to use any external server in Automate HA. Using an external server will lose the Automate HA functionalities, and things may not work as expected.

### Access

-   AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) with `AmazonS3FullAccess` and `AdministratorAccess` privileges are needed.

-   **On Bastion Machine:**
    -   We need a local user `hab` and local group `hab` linked together to complete the deployment process successfully.
    -   If they are unavailable, the SSH user should have privileges to create local users and groups so that the deployment process can create the required local user `hab` and local group `hab`.
    -   We only support local Linux users and groups for Installation flow. We don't support AD or LDAP managed users in nodes.
-   The SElinux config should either be disabled or permissive in the AMI Image used for deployment in config.

### Storage Space for Bastion

-   Operating System Root Volume (`/`) must be at least 40GB. Temporary space (`/var/tmp`) must be at least 10GB.
-   Separate Hab volume should be provisioned and mounted at `/hab` with at least 200GB space.

### SSH User

-   SSH Key Pair should be created in AWS, creating Bastion machine using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)
-   SSH users should use key-based SSH login without a passphrase.
-   The user's SSH key should be generated using algorithm `ed25519` without a passphrase.
-   This SSH user should be a local Linux user on the Bastion machine.
-   This SSH user should have sudo privileges on the Bastion machine.
-   Use the SSH user to access all machines using the same SSH private key.

### Cluster Setup

-   DNS certificate should be available in AWS Certificate Manager (ACM) for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
-   DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
-   DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.
-   During deployment, the Domain Certificates from ACM will be attached to the new Load Balancers.

### Config Changes

-   [Config Patch](/automate/ha_config/#patch-configuration/) in the whole application will result in downtime. For example, if you change or update something in OpenSearch or PostgreSQL, they will restart, resulting in restarting everything.
-   [Certificate Rotation](/automate/ha_cert_rotation/) will also change the system's configuration, leading to restarting the whole application.

To learn more about the above deployment, visit our [AWS deployment](/automate/ha_aws_deploy_steps/) page.

## External Managed Databases

Setup the following databases with password-based authentication.

### AWS Managed

-   AWS RDS PostgreSQL: 13.5
-   AWS OpenSearch: 1.3

Configure the backup only with **S3** when using AWS managed databases.

## Upgrade

Things to keep in mind while upgrading are:

-   Backend upgrades will restart the backend service, which takes time for the cluster to be healthy.
-   Upgrade command currently supports only minor upgrades.
-   A downtime will occur while upgrading the **frontend**, **backend** or the **workspace**.
-   Rolling upgrades are not supported.

## Disaster Recovery

Chef Automate HA supports disaster recovery in active/passive mode. The primary cluster will be in active mode, and the disaster recovery cluster will be in passive mode.
Active/Active Disaster Recovery is not supported right now as we do not support streaming of data across clusters and automatic failover switching of clusters.

The requirements for disaster recovery setup (Active/Passive) are:

-   Two identical clusters located in different data centers or cloud provider regions.
-   Network Attached Storage (NAS) or Object Store (S3) should be available in both data centers/regions.
-   Set up scheduled jobs to run backup and restore commands on both clusters. We recommend using **cron** to schedule the jobs.

To know more about the AWS deployment disaster recovery, visit our [Disaster Recovery Setup](/automate/ha_disaster_recovery_setup/) page.

## Migration

### Common Notes

-   Migrations involve downtime depending on how much data you have and the type of setup you are running.
-   Migration cannot be done from more than 1 Standalone Automate, more than 1 Standalone Infra Server, or more than 1 Chef Backend to a Single Automate HA cluster.
-   Automate HA will always have Chef Automate and Chef Infra Server running in the cluster.
-   Chef Manage or Private Chef Supermarket customers should not migrate to Automate HA.

| Existing System   | Supported Setup Type                                                                           | Minimum Eligible System Version      | Maximum Eligible System Version      | Pre-requisite Before Migration                                                                                                                                                                                                                                                              |
| ----------------- | ---------------------------------------------------------------------------------------------- | ------------------------------------ | ------------------------------------ | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Chef Automate     | [Standalone](/automate/install/)                                                               | Automate 2020XXXXXX                  |                                      | To migrate to Managed OpenSearch Automate HA cluster, the current standalone Chef Automate version should be at most 4.3.0.                                                                                                                                                                 |
| Chef Backend      | [Chef Backend Cluster](/server/install_server_ha/)                                             | Backend 2.X and Infra Server 14.X    | Chef Infra Server 15.4.0             | Chef Backend using PostgreSQL storage for Cookbooks should only migrate to Automate HA.                                                                                                                                                                                                     |
| Chef Infra Server | [Standalone](/server/install_server/#standalone)<br />[Tiered](/server/install_server_tiered/) | Infra server 14.XXX                  | Chef Infra Server 15.4.0             | Chef Infra Server using PostgreSQL storage for Cookbooks should only migrate to Automate HA. |
| A2HA              | PS Lead A2HA On-Premise Deployment                                                             | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | The A2HA cluster-mounted backup file system should also be attached to Automate HA cluster.<br />In case of In-Place migration, the volume having `/hab` should have more than 60% free space on each node.                                                                                 |

{{< note >}}

-   We do not support migration to Automate HA, if you have done any modification to the standard installation setup mentioned above.
-   We do not recommend in-place migration of A2HA and Chef Backend to Automate HA as the system levels change as ports, system users, and groups may conflict with the successful installation of Automate HA. Also, no easy rollback process is available. This may lead to higher downtime or loss of the existing setup.

{{< /note >}}

## Backup and Restore

-   **In AWS Deployment:** We support [**Elastic File System (EFS)**](/automate/ha_backup_restore_aws_efs/) or [**S3 storage)**](/automate/ha_backup_restore_aws_s3/) for taking backup.
-   **In AWS Managed Services:** We only support [**S3 storage**](/automate/ha_backup_restore_aws_s3/) for taking backup.
-   **In AWS Managed Services:** Create the below attributes by following [managed services documentation](/automate/managed_services/#enabling-opensearch-backup-restore)
    -   `aws_os_snapshot_role_arn`
    -   `os_snapshot_user_access_key_id`
    -   `os_snapshot_user_access_key_secret`

Encrypted S3 bucket are supported with only Amazon S3 managed keys (SSE-S3).
