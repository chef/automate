+++
title = "AWS Deployment Prerequisites"

draft = false

gh_repo = "AWS Deployment Prerequisites"
[menu]
  [menu.automate]
    title = "AWS Deployment Prerequisites"
    parent = "automate/deploy_high_availability"
    identifier = "automate/settings/ha-aws-deployment-prerequisites.md AWS Deployment Prerequisites"
    weight = 13
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}
The below prerequisites are according to the standard Chef Automate HA setup. You can contact the customer success manager or account manager if you use any specified version not mentioned here or a third-party extension or software.
{{< /warning >}}

Before installing Chef automate HA in AWS deployment mode, ensure you have taken a quick tour of this prerequisite page.

## Software Requirements

The software requirements of the nodes in the cluster and the other external Chef and non Chef tools will he discussed below:

### Node Software Requirements

The operating system and the supported version for different nodes in aws deployment of Automate HA is mentioned below:

| Operating Systems                        | Supported Version         |
| :--------------------------------------  | :-----------------------  |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x, 20.04.x |
| Centos (64 Bit OS)                       | 7                         |
| Amazon Linux 2 (64 Bit OS)               | 2 (kernel 5.10)           |
| SUSE Linux Enterprise Server 12 SP5      | 12                        |

{{< note >}} Chef Automate HA comes with bundled Infra Server, and it is recommended not to use any external server in Automate HA. Using an external server will lose the Automate HA functionalities, and things may not work as expected. {{< /note >}}

### Minimum Supported Chef Tool Versions

Current Automate HA supports integration with following Chef tools:

- Chef Infra Server version: 14.0.58+
- Chef Inspec version: 4.3.2+
- Chef Infra Client: 17.0.242+
- Chef Habitat: 0.81+

## Hardware Requirements

{{< note >}} Use a [Hardware Calculator](/calculator/automate_ha_hardware_calculator.xlsx) to check how much hardware you will need for your use case. {{< /note >}}

We have some sample values based on the performance benchmarking tests to give you an apt hardware configuration. Refer to the table below to populate things in the **Hardware Calculator** according to your requirement. The table below is just based on the tested **assumptions** and has no exact value.

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

- For **OpenSearch** and **PostgresSQL**, a minimum of three node clusters is required.
- For production, OpenSearch volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans.
{{< /note >}}

### Load Balancer

LoadBalancers in AWS deployment are set up according to [Chef Automate HA Architecture](/automate/ha/).

You can setup your [load balancer](/automate/loadbalancer_configuration/) using:

- [NGINX](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
- [HA Proxy](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)

## Firewall Checks

The Chef Automate High Availability (HA) cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. All the port configurations will be provisioned automatically.

**Ports required for Bastion**

| Machines | Bastion |
|----------|---------|
| Incoming | TCP 22  |
| Outgoing | TCP All |

{{< note >}} Custom SSH port is supported, but use the same port across all the machines. {{< /note >}}

**Port usage definitions**

| Protocol | Port Number | Usage                                                                                            |
|----------|-------------|--------------------------------------------------------------------------------------------------|
| TCP      | 22          | SSH to configure services                                                                        |
| TCP      | 9631        | Habitat HTTP API                                                        |
| TCP      | 443         | Allow Users to reach UI / API                                                                    |
| TCP      | 80          | Optional, Allows users to redirect to 443                                                        |
| TCP      | 9200        | OpenSearch API HTTPS Access                                                                      |
| TCP      | 9300        | Allows OpenSearch node to distribute data in its cluster.                                        |
| TCP/UDP  | 9638        | Habitat gossip (UDP) |
| TCP      | 7432        | HAProxy, which redirects to Postgresql Leader |
| TCP      | 6432        | Re-elect Postgresql Leader if Postgresql leader is down |

We do not support **Chef Manage** and **Supermarket** integration in ongoing Automate version.

## Certificates

A security certificate is a small data file used as an Internet security technique to establish a website or web application's identity, authenticity, and reliability. To ensure optimal security, rotate the certificates periodically.

Install an OpenSSL utility to create a self-signed key and certificate pair. Automate HA supports SSL certificates of type **PKCS 8**. Click [here](/automate/ha_cert_selfsign/#creating-a-certificate) to generate your certificate.

## Deployment Specific Pre-requisites

The AWS deployment specific pre-requisites are as follows:

- Create the Virtual Private Cloud (VPC) should with an internet gateway attached in AWS before starting. Reference for [VPC and CIDR creation](/automate/ha_vpc_setup/)
- If you want to use Default VPC, we must create public and private subnets. If the subnet is not available. Please refer [to this](https://docs.aws.amazon.com/vpc/latest/userguide/default-vpc.html)
- We need three private and three public subnets in a vpc (1 subnet for each AZ). As of now, we support dedicated subnets for each AZ.
- We recommend creating a new vpc. And Bastion should be in the same VPC.
- Get AWS credentials (`aws_access_key_id` and `aws_secret_access_key`) with privileges like: `AmazonS3FullAccess`, and `AdministratorAccess`. \
    Set these in `~/.aws/credentials` in Bastion Host:

    ```bash
    sudo su -
    ```

    ```bash
    mkdir -p ~/.aws
    echo "[default]" >>  ~/.aws/credentials
    echo "aws_access_key_id=<ACCESS_KEY_ID>" >> ~/.aws/credentials
    echo "aws_secret_access_key=<SECRET_KEY>" >> ~/.aws/credentials
    echo "region=<AWS-REGION>" >> ~/.aws/credentials
    ```

- Have DNS certificate ready in ACM for 2 DNS entries: Example: `chefautomate.example.com`, `chefinfraserver.example.com`
    Reference for [Creating new DNS Certificate in ACM](/automate/ha_aws_cert_mngr/)
- Have SSH Key Pair ready in AWS, creating new VMs using that pair.
    Reference for [AWS SSH Key Pair creation](https://docs.aws.amazon.com/ground-station/latest/ug/create-ec2-ssh-key-pair.html)
- We do not support passphrases for Private Key authentication.
- Preferred key type will be ed25519.
- DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.

{{< warning >}}

- PLEASE DO NOT MODIFY THE WORKSPACE PATH. It should always be "/hab/a2_deploy_workspace"
- We currently don't support AD managed users in nodes. We only support local Linux users.

{{< /warning >}}

Click [here](/automate/ha_aws_deploy_steps/) to know more.

## Upgrade

Things to keep in mind while upgrading are:

- Backend upgrades will restart the backend service, which takes time for the cluster to be healthy.
- Upgrade command currently supports only minor upgrades.
- A downtime might occur while upgrading the **frontend**, **backend** or the **workspace**.

## Config Updates

Patching something in the application might result in downtime of the whole application. For example, if you change or update something in OpenSearch or Postgres, they will restart, resulting in restarting everything in the front end.

Click [here](/automate/ha_config/#patch-configuration/) to learn more about how to patch the configs.

## Disaster Recovery

The requirement to set up a recovery point objective is:

- Two identical clusters located in different data centers or cloud provider regions.
- Network accessible storage (NAS) and object store (S3), available in both data centers/regions
- Ability to schedule jobs to run backup and restore commands in both clusters. We recommend using **cron** or a tool like **anacron**.
- The Primary cluster will be active and Disaster cluster will be in passive mode.

Click [here](/automate/ha_disaster_recovery_setup/) to learn more about the AWS deployment disaster recovery cluster.

## Migration

| Existing System | Minimum Eligible System Version | Maximum Eligible System Version |  Pre-requisite Before Migration | Notes | Not Supported Use Cases |
|-----------------|---------------------------------|-------|------------------------------| ----- | ----------------------- |
| Chef Automate | Automate 2020XXXXXX |    |   | Migrations involve downtime depending on data and the setup. | Chef Automate users running Chef Infra Server in external mode should not migrate to Automate HA. |
| Chef Backend | Backend 2.x and Infra Server 14.x |   |    | Irrespective of whether you use to automate or not, automate nodes will be actively running in automate HA cluster |  Chef Manage or Private Chef Supermarket with Chef Backend should not migrate with this. |
| Chef Infra Server | Infra server 14.xxx |   |    | Irrespective of whether you use to automate or not, automate nodes will be actively running in automate HA cluster |  Chef Manage or Private Chef Supermarket with Chef Backend should not migrate with this. Automate HA does not support supermarket authentication with chef-server user credentials. |
| A2HA | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | Your machine should be able to mount the file system, which was mounted to the A2HA cluster for backup purposes, to Automate HA. Configure the A2HA to take backup on a mounted network drive (location example: /mnt/automate_backup). | Migrations involve downtime depending on data and the setup |    |
| In-Place A2HA | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | A healthy state of the A2HA cluster to take fresh backup. A2HA is configured to take backup on a mounted network drive (location example: /mnt/automate_backup). Availability of 60% of space. | Migrations involve downtime depending on data and the setup |    |

## Backup and Restore

In AWS deployment of Automate HA, we support [**Elastic File System (EFS)**](/automate/ha_backup_restore_aws_efs/) or [**Object Storage (S3/MinIO)**](/automate/ha_backup_restore_aws_s3/) for taking backup.

In AWS with Managed Services deployment of Automate HA, we support [**Object Storage (S3/MinIO)**](/automate/ha_backup_restore_aws_s3/) for taking backup.