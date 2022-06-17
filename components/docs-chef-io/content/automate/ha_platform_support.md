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
| Amazon Linux 2 (64 Bit OS)               | 2 (kernel 5.10)           |

## Hardware Requirements

We get these results based on our performance benchmarking tests according to the assumptions below.

### Assumption
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

| Instance          | Count | vCPU | RAM | Storage Size | AWS Machine Type |
|-------------------|-------|------|-----|--------------|------------------|
| Chef Automate     | 2     | 2    | 8   | 80 GB        | m5.large         |
| Chef Infra Server | 2     | 2    | 8   | 80 GB        | m5.large         |
| Postgresql DB     | 3     | 2    | 8   | 150 GB       | m5.large         |
| Opensearch DB     | 3     | 2    | 8   | 58.9 GB      | m5.large         |
| Bastion Machine   | 1     | 2    | 8   | 150 GB       | m5.large         |

The configuration is based on the assumptions listed above. Using the sample calculation given [here](/calculator/automate_ha_hardware_calculator.xlsx), you can calculate it based on the actual estimate.

{{< note >}}

- For **OpenSearch** and **PostgresSQL**, a minimum of three node clusters is required.
- For production, OpenSearch volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans.

{{< /note >}}
