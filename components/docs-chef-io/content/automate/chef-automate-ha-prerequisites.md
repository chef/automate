+++
title = "Pre-Requisites"

draft = false

gh_repo = "Pre-Requisites"
[menu]
  [menu.automate]
    title = "Pre-Requisites"
    parent = "automate/deploy_high_availability"
    identifier = "automate/settings/chef-automate-ha-prerequisites.md Pre-Requisites"
    weight = 12
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

Before installing Chef automate HA, ensure you have taken a quick tour of this pre-requisite page.

## Platform Support

This section lists the recommended requirements for operating systems, virtual machine instances, and VPC for implementing the Chef Automate High Availability (HA) in your network infrastructure.

## Software Requirements

| Operating Systems                        | Supported Version         |
| :--------------------------------------  | :-----------------------  |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x, 20.04.x |
| Centos (64 Bit OS)                       | 7                         |
| Amazon Linux 2 (64 Bit OS)               | 2 (kernel 5.10)           |
| SUSE Linux Enterprise Server 12 SP5      | 12                        |

{{< note >}} [Hardware Calculator](/calculator/automate_ha_hardware_calculator.xlsx) use this to check how much hardware you will need for your use-case. {{< /note >}}

## Hardware Requirements

The hardware configuration is according to the performance benchmarking tests based on the assumptions listed below:

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
- Chef Automate bundle comes with chef-server version 14.15.10
{{< /note >}}

The Automate HA supports three types of deployment:

- On-Premise Deployment
- AWS Deployment
- AWS Managed Services Deployment

The below requirements are elaborated according to the above three deployments.

## On-Premise Deployment Pre-Requisite

The on-premise deployment specific pre-requisites are given below:

- All VMs or Machines are up and running.
- OS Root Volume (/) must be at least 40 GB
- TMP space (/var/tmp) must be at least 5GB
- Separate Hab volume (/hab) provisioned at least 100 GB for OpenSearch node `/hab` volume will be more based on the data retention policy.
- A Common user has access to all machines.
- This common user should have sudo privileges.
- This common user uses the same SSH Private Key file to access all machines.
- Key-based SSH for the provisioning user for all the machines for HA-Deployment.
- We do not support passphrases for Private Key authentication.
- LoadBalancers are set up according to [Chef Automate HA Architecture](/automate/ha/) needs as explained in [Load Balancer Configuration page](/automate/loadbalancer_configuration/).
- Network ports are opened as per [Chef Automate Architecture](/automate/ha/) needs as explained in [Security and Firewall page](/automate/ha_security_firewall/)
- DNS is configured to redirect `chefautomate.example.com` to the Primary Load Balancer.
- DNS is configured to redirect `chefinfraserver.example.com` to the Primary Load Balancer.
- Certificates are created and added for `chefautomate.example.com`, and `chefinfraserver.example.com` in the Load Balancers.
- If DNS is not used, add the records to `/etc/hosts` in all the machines, including Bastion:

    ```bash
    sudo sed '/127.0.0.1/a \\n<Primary_LoadBalancer_IP> chefautomate.example.com\n<Primary_LoadBalancer_IP> chefinfraserver.example.com\n' -i /etc/hosts
    ```

- If the instance is **RedHat**, set SElinux config `enforcing` to `permissive` in all the nodes. SSH to each node, then run:

    ```bash
    sudo sed -i 's/SELINUX=enforcing/SELINUX=permissive/g' /etc/selinux/config
    ```

Click [here](/automate/ha_onprim_deployment_procedure/) to know more.

### Load Balancer

LoadBalancers in on-premise deployment are set up according to [Chef Automate HA Architecture](/automate/ha/).

You can setup your [load balancer](/automate/loadbalancer_configuration/) using:

- [NGINX](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
- [HA Proxy](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)

### Security Checks

HA cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. Click [here](/automate/ha_security_firewall/#ports-required-for-all-machines) for the breakdown of those ports and what needs to be open for each set of servers.

Automate HA supports custom SSH port but the same port should be used accors all the machines.

### Disaster Recovery

The requirement to setup a recovery point objective is:

- Two identical clusters located in different data centers or cloud provider regions.
- Network accessible storage (NAS), object store (S3), available in both data centers/regions
- Ability to schedule jobs to run backup and restore commands in both clusters. We recommend using corn or a similar tool like anacron.

Click [here](/automate/ha_disaster_recovery_setup/) to know more on the disaster recovery cluster for om-premise deployment.

### Certificates

A security certificate is a small data file used as an Internet security technique through which the identity, authenticity, and reliability of a website or web application are established. To ensure optimal security, rotate the certificates periodically.

Install an OpenSSL utility to create a self-signed key and certificate pair. The certificates used for SSL is **PKCS 8**. Click [here](/automate/ha_cert_selfsign/) to know more.

### Privileges

The Chef Automate HA user gets the privilege to access the habitat directory. The linux user should have a `/hab` and `/temp` directory.

### Chef Application Minimum Version

The minimum version for the chef applications are as follows:

- Chef Server: **chef/automate-cs-oc-erchef/15.4.0/20230130152857**
- Chef Habitat: **core/hab/1.6.521/20220603154827**
- Chef Automate: **chef/deployment-service/0.1.0/20230213182348**
- Infra Client:
- Chef Manage & Supermarket:

### Backup and Restore

On-premise deployment can take place using **Filesystem** and **Object Storage**. If you choose `backup_config` as filesystem or object storage in your `config.toml` file, the backup is already configured during the deployment. Whereas, if the `backup_config` is left black, then the configuration needs to be done manually. Click [here](/automate/ha_backup_restore_file_system/) to know more.

### Migration

The on-Premise deployment supports four types of migration:

- **Existing A2HA to Automate HA**

    {{< note >}} A2HA user can be migrated to Automate HA with a minimum Chef Automate version [20201230192246](https://docs.chef.io/release_notes_automate/#20201230192246). {{< /note >}}

    To migrate your existing A2HA data to the newly deployed Chef Automate HA, firstly:

    - Your machine should have the ability to mount the file system, which was mounted to A2HA cluster for backup purposes, to Automate HA.
    - Configure the A2HA to take backup on a mounted network drive (location example: `/mnt/automate_backup`).

Click [here](/automate/ha_existing_a2ha_to_automate_ha/) to know more about the process of migration.

- **In-Place A2HA to Automate HA**

    To migrate your in-place A2HA to Automate HA, firstly you should have:

    - A healthy state of the A2HA cluster to take fresh backup.
    - A2HA is configured to take backup on a mounted network drive (location example: `/mnt/automate_backup`).
    - Availability of 60% of space.

Click [here](/automate/ha_inplace_migration/) to know more about the process of migration.

- **Chef Backend to Automate HA**

    - Customers using only **Chef Backend** are advised to follow this migration guidance. Customers using **Chef Manage** or **Private Chef Supermarket** with Chef Backend should not migrate with this.
    - Automate HA do not support the super market authentication with chef-server users credentials.
    - Post Migration Customer can not login with chef-server users to Supermarket.

Click [here](/automate/ha_chef_backend_to_automate_ha/) to know more about the process of migration.

- **Automate to Automate HA**

    - Standalone Chef Automate or Chef Automate with embedded Chef Infra Server can migrate to Automate HA, with minimum version of Chef Automate: [20201230192246](https://docs.chef.io/release_notes_automate/#20201230192246)

    - Chef Automate user running Chef Infra Server in external mode should not migrate to Automate HA.

Click [here](/automate/ha_automate_to_automate_ha/) to know more about the process of migration.
