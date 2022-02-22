+++
title = "High Availability - System and Software Requirements"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "High Availability - System and Software Requirements"
    parent = "automate/install"
    identifier = "automate/install/ha_system_requirements.md HA System and Software Requirements"
    weight = 200
+++

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
