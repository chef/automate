+++
title = "Platform Support"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Platform Support"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/deploy_high_availability/ha_system_requirements/ha_platform_support.md Platform Support"
    weight = 200
+++

This section lists the recommended requirements for operating systems, virtual machine instances, and VPC for implementing the Chef Automate High Availability (HA) in your network infrastructure.

| Operating Systems                        | Supported Version         |
| :--------------------------------------  | :-----------------------  |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8. For 8 or above versions, the **SELinux** configuration must be permissive. The **SELinux** configuration is enforced in RHEL 8. Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x          |
| Centos (64 Bit OS)                       | 7                         |

## Hardware Configuration

Based on the number of nodes, the virtual machine requirements for test instances are as follows:

| Instance          | Count | vCPU | RAM   | Volume Size (dedicated hard disk space assigned to '/') |
| :---------------  | :---- | :--- |:------| :-----------------------------------------------------  |
| PostgreSQL        | 3     | 2    | 8 GB  | 50 GB                                                   |
| OpenSearch        | 3     | 4    | 16 GB | 50 GB                                                   |
| Chef Automate     | 2     | 2    | 8 GB  | 50 GB                                                   |
| Chef Infra Server | 2     | 2    | 8 GB  | 50 GB                                                   |

The above hardware configuration can be used to test up to 5000 nodes.

{{< note >}}

- For **OpenSearch** and **PostgresSQL**, a minimum of three node clusters is required.
- For production ES volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans.

{{< /note >}}

The hardware calculation formule for the production set up is given below:

### Calculation for Volume Size for OpenSearch

- **Data from Compliance Scan** = Number of Scans per Day X Approx Size of Data X Retention Days X Number of Nodes X 2(OpenSearch Replica)
- **Data from Client Run** = Number of Runs per Day X Approx Size of Data X Retention Days X Number of Nodes * 2(OpenSearch Replica)
- **Total data** = Data from Compliance Scan + Data from Client Run
OpenSearch volume for each instance = **Total Data / 3**

### Calculation for vCPU and RAM vs CCR
