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
The below prerequisites are according to our organizational standard. If you are using any specified version not mentioned here or a third-party extension or software, you can reach out to the CAMs and our Customer Support Team.
{{< /warning >}}

Before installing Chef automate HA in On-premise deployment mode, ensure you have taken a quick tour of this pre-requisite page.

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

LoadBalancers in on-premise deployment are set up according to [Chef Automate HA Architecture](/automate/ha/).

You can setup your [load balancer](/automate/loadbalancer_configuration/) using:

- [NGINX](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
- [HA Proxy](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)

## Software Requirements

| Operating Systems                        | Supported Version         |
| :--------------------------------------  | :-----------------------  |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x, 20.04.x |
| Centos (64 Bit OS)                       | 7                         |
| Amazon Linux 2 (64 Bit OS)               | 2 (kernel 5.10)           |
| SUSE Linux Enterprise Server 12 SP5      | 12                        |

{{< note >}} Chef Automate HA comes with bundled Infra Server, and it is recommended not to use any external server in Automate HA. Using an external server will lose the Automate HA functionalities, and things may not work as expected. {{< /note >}}

## Deployment Specific Pre-requisites

The on-premise deployment specific pre-requisites are as follows:

### Access

- All Virtual Machines or Machines should be up and running.
- We need a local user `hab` and local group `hab` linked together to complete the deployment process successfully.
- If they are unavailable, the SSH user should have privileges to create local users and groups so that the deployment process can create the required local user `hab` and local group `hab`.

### Storage Space

- Operating System Root Volume (`/`) must be at least 40 GB. Temporary space (`/var/tmp`) must be at least 5GB.
- Separate Hab volume should be provisioned and mounted at `/hab` with at least 100GB free space for all nodes except OpenSearch.
- For OpenSearch nodes, hab volume should be calculated based on data retension policy and [hardware calculator](/calculator/automate_ha_hardware_calculator.xlsx) can be used for estimation.

### SSH User

- SSH user should use key based SSH login without passphrase.
- The user's SSH key should be generated using algorithm `ed25519` and `RSA(2048)`without passphrase.
- This SSH user should be local Linux user on all the machines.
- This SSH user should have sudo privileges on all the machines.
- The SSH user should use same SSH private key to access all machines.

### Cluster setup

- LoadBalancers should be setup according to [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/).
- Network ports should be opened as per [Chef Automate HA Architecture](/automate/ha/#chef-automate-ha-architecture/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/).
- DNS is configured to redirect chefautomate.example.com to the Primary Load Balancer.
- DNS is configured to redirect chefinfraserver.example.com to the Primary Load Balancer.
- Certificates are created and added for chefautomate.example.com, and chefinfraserver.example.com in the Load Balancers.







The on-premise deployment specific pre-requisites are as follows:

- All Virtual Machines or Machines are up and running.
- OS Root Volume (/) must be at least 40 GB.
- TMP space (/var/tmp) must be at least 5GB.
- Separate Hab volume (/hab) provisioned at least 100 GB for OpenSearch node `/hab` volume will be more based on the data retention policy.
- Create a common SSH user that has access to all the machines with sudo privileges.
- The above SSH user should have the write access to `/hab` and `/temp` directory.
- This common user uses the same SSH Private Key file to access all machines.
- We only support local Linux users and local Linux groups.
- We need to have private key authentications for all the VMs, and the private keys should be generated without any passphrases.
- We need a local hab user and local hab group to complete the deployment process successfully. If they are unavailable, the common user should have privileges to create local users and groups so that the deployment process can create the required local users and groups.
- The provisioning users should use Key-based SSH on all the machines for HA-Deployment. Chef recommends you use an SSH key of size 2048 bits.
- LoadBalancers are set up according to [Chef Automate HA Architecture](/automate/ha/) needs as explained in [Load Balancer Configuration page](/automate/loadbalancer_configuration/).
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/)
- DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.
- Certificates are created and added for `chefautomate.example.com`, and `chefinfraserver.example.com` in the Load Balancers.
- If DNS is not used, add the records to `/etc/hosts` in all the machines, including Bastion:

    ```bash
    sudo sed '/127.0.0.1/a \\n<Primary_LoadBalancer_IP> chefautomate.example.com\n<Primary_LoadBalancer_IP> chefinfraserver.example.com\n' -i /etc/hosts
    ```

- The SElinux config should either be disabled or permissive.

{{< warning >}}

- PLEASE DO NOT MODIFY THE WORKSPACE PATH. It should always be "/hab/a2_deploy_workspace"
- We currently don't support AD managed users in nodes. We only support local Linux users.

{{< /warning >}}

Click [here](/automate/ha_onprim_deployment_procedure/) to know more.

### Firewall Checks

The Chef Automate High Availability (HA) cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. Below is a breakdown of those ports and what needs to be opened for each set of servers.

**Ports required for all Machines**

| Machines | Chef Automate         | Chef Infra Server     | Postgresql                                  | OpenSearch                                  | Bastion      | Load Balancer |
|----------|-----------------------|-----------------------|---------------------------------------------|---------------------------------------------|--------------| ---------- |
| Incoming | TCP 22, 9631, 443, 80 | TCP 22, 9631, 443, 80 | TCP 22, 9631, 7432, 5432, 9638<br/>UDP 9638 | TCP 22, 9631, 9200, 9300, 9638, 6432<br/>UDP 9638 | TCP 22       | TCP 443, 80 |
| Outgoing | TCP 22, 9631, 443, 80 | TCP 22, 9631, 443, 80 | TCP 22, 9631, 7432, 5432, 9638<br/>UDP 9638 | TCP 22, 9631, 9200, 9300, 9638, 6432<br/>UDP 9638 | TCP 22, 9631 | TCP 443, 80 |

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

### Disaster Recovery

The requirement to set up a recovery point objective is:

- Two identical clusters located in different data centers or cloud provider regions.
- Network accessible storage (NAS) and object store (S3), available in both data centers/regions
- Ability to schedule jobs to run backup and restore commands in both clusters. We recommend using **cron** or a tool like **anacron**.
- The Primary cluster will be active and Disaster cluster will be in passive mode.

Click [here](/automate/ha_disaster_recovery_setup/) to learn more about the on-premise deployment disaster recovery cluster.

### Custom Certificates

A security certificate is a small data file used as an Internet security technique to establish a website or web application's identity, authenticity, and reliability. To ensure optimal security, rotate the certificates periodically.

Install an OpenSSL utility to create a self-signed key and certificate pair. Automate HA supports SSL certificates of type **PKCS 8**. Click [here](/automate/ha_cert_selfsign/#creating-a-certificate) to generate your certificate.

### Minimum Supported Chef Tool Versions

- Chef Infra Server version: 14.0.58+
- Chef Inspec version: 4.3.2+
- Chef Infra Client: 17.0.242+
- Chef Habitat: 0.81+

We do not support **Chef Manage** and **Supermarket** integration in ongoing Automate version.

### Backup and Restore

On-premise deployment can use **Filesystem** and **Object Storage** and if you choose `backup_config` as the filesystem or object storage in your `config.toml` file, the backup gets configured during the deployment. If the `backup_config` is left black, configure it manually. Click [here](/automate/ha_backup_restore_file_system/) to know more.

For backup and restore from standalone to HA, there are two conditions:

1. Register the OS snapshot to the same path in HA as in standalone.
1. The s3 repository configured for backup in HA should be the same as the standalone.

To make sure the restore happens successfully, we need to:

1. Delete the snapshots from the HA setup if it's different from the standalone.
1. Make sure the same s3 repository is configured in HA.
1. In the `--patch-config`, which we pass in the restore command, ensure that the config has the same basepath under `external.os` section and the `backup` section as its there in standalone

### Upgrade

Things to keep in mind while upgrading are:

- Backend upgrades will restart the backend service, which takes time for the cluster to be healthy.
- Upgrade command currently supports only minor upgrades.
- A downtime might occur while upgrading the **frontend**, **backend** or the **workspace**.

### Config Updates

Patching something in the application might result in downtime of the whole application. For example, if you change or update something in OpenSearch or Postgres, they will restart, resulting in restarting everything in the front end.

Click [here](/automate/ha_config/#patch-configuration/) to learn how to patch the configs.

### External Managed Databases

| External Managed Database | PostgreSQL | OpenSearch |
| ----- | ----- | ------ |
| AWS Managed | AWS RDS Postgresql 13.5 | AWS OpenSearch 1.3 |
| Customer Managed | Postgresql 13.5 | OpenSearch 1.3.7 |

### Migration

| Existing System | Minimum Eligible System Version | Maximum Eligible System Version |  Pre-requisite Before Migration | Notes | Not Supported Use Cases |
|-----------------|---------------------------------|-----------|------------------------------| ----- | ----------------------- |
| Chef Automate | Automate 2020XXXXXX |    |   | Migrations involve downtime depending on data and the setup. | Chef Automate users running Chef Infra Server in external mode should not migrate to Automate HA. |
| Chef Backend | Backend 2.x and Infra Server 14.x |   |    | Irrespective of whether you use to automate or not, automate nodes will be actively running in automate HA cluster |  Chef Manage or Private Chef Supermarket with Chef Backend should not migrate with this. |
| Chef Infra Server | Infra server 14.xxx |   |    | Irrespective of whether you use to automate or not, automate nodes will be actively running in automate HA cluster |  Chef Manage or Private Chef Supermarket with Chef Backend should not migrate with this. Automate HA does not support supermarket authentication with chef-server user credentials. |
| A2HA | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | Your machine should be able to mount the file system, which was mounted to the A2HA cluster for backup purposes, to Automate HA. Configure the A2HA to take backup on a mounted network drive (location example: /mnt/automate_backup). | Migrations involve downtime depending on data and the setup |    |
| In-Place A2HA | Chef Automate version 20201230192246 | Chef Automate Version 20220223121207 | A healthy state of the A2HA cluster to take fresh backup. A2HA is configured to take backup on a mounted network drive (location example: /mnt/automate_backup). Availability of 60% of space. | Migrations involve downtime depending on data and the setup |    |
