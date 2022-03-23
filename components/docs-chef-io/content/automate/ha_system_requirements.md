+++
title = "System and Software Requirements"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "System and Software Requirements"
    parent = "automate/deploy_high_availability/introduction"
    identifier = "automate/deploy_high_availability/introduction/ha_system_requirements.md System and Software Requirements"
    weight = 240
+++
This section lists the recommended requirements for operating systems, virtual machine instances, and VPC for implementing the Chef Automate High Availability (HA) in your network infrastructure.

## Platform support

| Operating Systems                        | Tested                    |
| :--------------------------------------: | :-----------------------: |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, **SELinux** configuration must be permissive. By default, the **SELinux** configuration is enforced in RHEL 8). Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 |packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x          |
| Centos (64 Bit OS)                       | 7                         |

## Virtual Machine (VM) Instances Type

Based on the number of nodes, the virtual machine requirements are as follows:

| Instance          | RAM               | Volume Size                                        |
| :---------------: | :---------------- | :------------------------------------------------: |
| PostgreSQL        | 4 GB RAM for test | 50 GB (dedicated hard disk space assigned to '/'). |
| Elasticsearch     | 8 GB RAM for test | 50 GB (dedicated hard disk space assigned to '/'). |
| Chef Automate     | 4 GB RAM for test | 50 GB (dedicated hard disk space assigned to '/'). |
| Chef Infra Server | 4 GB RAM for test | 50 GB (dedicated hard disk space assigned to '/'). |

{{< note >}}

ES volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans. The above table includes instances’ RAM and volume size, set up for the testing purpose. Production depends on the number of nodes and the frequency of Chef Infra Client runs and compliance scans. However, for on-premises deployment, you can choose the above requirements for VM like RAM.

For **Elasticsearch** and **PostgresSQL**, a minimum of three node clusters is required.

{{< /note >}}
