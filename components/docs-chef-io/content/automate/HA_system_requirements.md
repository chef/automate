+++
title = "HA - System and Software Requirements"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - System and Software Requirements"
    parent = "automate/High_Availability"
    identifier = "automate/reference/HA_system_requirements.md HA System and Software Requirements"
    weight = 40
+++

## System & Software Requirements

This section lists the operating systems requirements, virtual machine instances requirements, and VPC requirements for implementing Chef Automate High Availability (HA) for your network infrastructure or systems or applications or services.

## Platform support

+---------------------------------------+------------------------------------------------------------------+
|      Operating Systems                |     Tested                                                       |
|      :----------------:               |     :-----:                                                      |
+=======================================+==================================================================+
| Red Hat Enterprise Linux (64 Bit OS)  | 7, 8 (For 8 or above versions, **SELinux** configuration must    |
|                                       | be permissive. By default, in RHEL 8, **SELinux** configuration  |
|                                       | is enforced). Red Hat Enterprise Linux derivatives include       |
|                                       | Amazon Linux v1 (using RHEL 6   |packages) and v2 (using RHEL 7  |
|                                       | packages).                                                       |
+---------------------------------------+------------------------------------------------------------------+
| Ubuntu (64 Bit OS)                    |  14.04.x, 16.04.x, 18.04.x, 20.04.x                              |
+---------------------------------------+------------------------------------------------------------------+
| Centos (64 Bit OS)                    |  7                                                               |
+---------------------------------------+------------------------------------------------------------------+

## Virtual Machine (VM) Instances Type

+---------------------+-------------------------+----------------------------------------+---------------------+
|   Instance          |       Type              |            RAM                         |  Volume-size        |
|  :----------------: |   :----------------:    |         :----------------:             |  :---------------:  |
+=====================+=========================+========================================+=====================+
| PostgreSQL          | t3.medium               | 4 GB RAM for test and 8 GB for         | 50 GB (dedicated    |
|                     |                         | production. vCPU - 2.                  | hard disk space     |
|                     |                         |                                        | need to be assigned |
|                     |                         |                                        | assigned to '/').   |
+---------------------+-------------------------+----------------------------------------+---------------------+
| Elasticsearch       | m5.large                | 8 GB RAM for test and 16 GB for        | 50 GB (dedicated    |
|                     |                         | production. vCPU - 2.                  | hard disk space     |
|                     |                         |                                        | need to be assigned |
|                     |                         |                                        | to '/').            |
+---------------------+-------------------------+----------------------------------------+---------------------+
| Automate            | t3.medium               | 4 GB RAM for test and 8 GB for         | 50 GB (dedicated    |
|                     |                         | production. vCPU - 2.                  | hard disk space     |
|                     |                         |                                        | need to be assigned |
|                     |                         |                                        | to '/').            |
+---------------------+-------------------------+----------------------------------------+---------------------+
| Chef Server         | t3.medium               | 4 GB RAM for test and 8 GB for         | 50 GB (dedicated    |
|                     |                         | production. vCPU - 2.                  | hard disk space     |
|                     |                         |                                        | need to be assigned |
|                     |                         |                                        | to '/').            |
+---------------------+-------------------------+-----------------------------------------+--------------------+

{{< notes >}}

ES volume size also depends on the number of nodes and frequency of client runs and compliance scans. The above table includes AWS instances types. However, for Bare-infra deployment or In-premises deployment types, you can choose the above requirements for VM like RAM.

For **ElasticSearch** and **postgres-sql**, a minimum of three node clusters is required.

{{< /note >}}
