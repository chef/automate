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
| Amazon Linux 2 (64 Bit OS)               | 2 (kernal 5.10)           |

## Hardware Requirments

### Assumption
- Compliance scan report size : 4 MB 
- Compliance Scan  : 4 times a  days
- Client Run (Infra run) size :  150 KB 
- Client runs (Infra runs) :  every 30 minutes : 48 times a day 
- All Scans are run staggered I.e.  spread out in a 30 minutes period 
- Concurrency expected between 400 Infra scan per minute  to 800 scan per minute 
- Data Retention policy  : 30 Days 
- Number of Nodes : 12,000

### Calculation for RAM 
- **Total RAM Required for Opensearch** = [Number of Scans per Day] x [Approx Size of Data] x [Number of Nodes] 
- **RAM required per opensearch node** = [Total RAM Required for Opensearch] / [Number of Opensearch Nodes]
- If [RAM required per opensearch node] more than 32 GB, then increase the number of nodes by odd number
- **Physical RAM required per Opensearch node**  = [RAM required per opensearch node] x 1.3 x 1.5
 
#### Sample Calculation for RAM requirments for Opensearch 
- **Total RAM Required for Opensearch** = (4 x 4 MB x 12000) ~ 187.5 GB 
- **RAM required per opensearch node** = (187.5 GB / 3) ~ 62.5 GB
- As RAM required per opensearch node is greater than 32 GB, thus number of node should be greater than 5 and next odd number is 7. Thus we take 7. 
- **RAM required per opensearch node** = (187.5 GB / 7) ~ 26.7 GB
- **Physical RAM required per Opensearch node**  = [RAM required per opensearch node] x 1.3 x 1.5 = 52 GB ~ 64 GB per node.  
- By calculation, number of opensearch node should be 7 with each 64 GB RAM.

### Calculation for vCPU (FrontEnd Nodes)

- **Number of Request per Automate node** = [Max conncurrency number per minute] / [Number of Automate nodes]
- Assumption **Time to process per request** = 1.2 Seconds
- **Number of vCPU per Automate node** = ( [Number of Request per Automate node] * [Time to process per request] ) / 60 / 2 (threads per cpu)

### Sample calculation for vCPU (FrontEnd Nodes)

- **Number of Request per Automate node** = 800 / 2
- Assumption **Time to process per request** = 1.2 Seconds
- **Number of vCPU per Automate node** = ( 400 * 1.2 ) / 60 / 2 (threads per cpu) ~ 4

### Calculation for Volume Size for OpenSearch

- **Data from Compliance Scan** = [Number of Scans per Day] x [Approx Size of Data] x [Retention Days] x [Number of Nodes] x 2(OpenSearch Replica)
- **Data from Client Run** = [Number of Runs per Day] x [Approx Size of Data] x [Retention Day] x [Number of Node] x 2(OpenSearch Replica)
- **Total Disc for Opensearch node** = ( [Data from Compliance Scan] + [Data from Client Run] + 10% extra Disc space ) + / 3 (number of opensearch nodes) 

#### Sample Calculation for the above assumptions
- **Data from Compliance Scan** =  4 * 4 MB * 30 Days * 12000 * 2 =  11 TB
- **Data from Client Run** = 48 * 150 KB * 30 Days * 12000 * 2 =  4.8 TB
- **Total Disc for Opensearch node** = 11 TB + 4.8 TB = 15.8 TB + 10% extra = (17.4 TB / 3) = 5.8 TB ~ 6 TB


Based on the above calculation machine requirment will be:

| Instance          | Count | vCPU | RAM   | Storage Size | AWS Machine type|
| :---------------  | :---- | :--- |:------|:------| :-----------------------------------------------------  |
| PostgreSQL        | 3     | 4    | 16 GB  | 150 GB | m5.xlarge |
| OpenSearch        | 7     | 8    | 64 GB  | 6 TB  | r5.2xlarge |
| Chef Automate     | 2     | 4    | 16 GB  | 100 GB | m5.xlarge |
| Chef Infra Server | 2     | 4    | 16 GB  | 100 GB | m5.xlarge |

The above hardware configuration is based on above assumption. Customer should calculate based on there actual estimate using the sample calculation provided. 

{{< note >}}

- For **OpenSearch** and **PostgresSQL**, a minimum of three node clusters is required.
- For production ES volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans.

{{< /note >}}


