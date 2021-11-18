+++
title = "Chef Automate HA - System and Software Requirements"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "HA - System and Software Requirements"
    parent = "automate/High_Availability"
    identifier = "automate/reference/ha_system_requirements.md HA System and Software Requirements"
    weight = 40
+++

## System & Software Requirements

This section lists the recommended operating systems requirements, virtual machine instances requirements, and VPC requirements for implementing Chef Automate High Availability (HA) for your network infrastructure or systems or applications or services.

## Platform support

| Operating Systems                        | Tested                    |
| :--------------------------------------: | :-----------------------: |
| Red Hat Enterprise Linux (64 Bit OS)     | 7, 8 (For 8 or above versions, **SELinux** configuration must be permissive. By default, in RHEL 8 **SELinux** configuration is enforced). Red Hat Enterprise Linux derivatives include Amazon Linux v1 (using RHEL 6 |packages) and v2 (using RHEL 7packages). |
| Ubuntu (64 Bit OS)                       | 16.04.x, 18.04.x, 20.04.x |
| Centos (64 Bit OS)                       | 7                         |

## Virtual Machine (VM) Instances Type

| Instance          | Type         | RAM                                                   | Volume-size                             |
| :---------------: | :----------: | :---------------------------------------------------: | :-------------------------------------: |
| PostgreSQL        | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). |
| Elasticsearch     | m5.large     | 8 GB RAM for test and 16 GB for production. vCPU - 2. | 50 GB (dedicated hard disk space assigned to '/'). |
| Chef Automate     | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). |
| Chef Infra Server | t3.medium    | 4 GB RAM for test and 8 GB for production. vCPU - 2.  | 50 GB (dedicated hard disk space assigned to '/'). |

{{< note >}}

ES volume size also depends on the number of nodes and frequency of Chef Infra Client runs and compliance scans. The above table includes AWS instances types. However, for Bare-infra deployment or In-premises deployment types, you can choose the above requirements for VM like RAM.

For **Elasticsearch** and **PostgresSQL**, a minimum of three node clusters is required.

{{< /note >}}

## Amazon's Virtual Private Cloud (VPC)

Amazon VPC, a virtual network dedicated to your AWS account that enables you to launch AWS resources into a virtual network. This virtual network resembles a traditional network that you had operate in your own data center, with the benefits of using the scalable infrastructure of AWS.

Amazon VPC is the networking layer for Amazon EC2. 

Amazon Elastic Compute Cloud (Amazon EC2) provides scalable computing capacity in the Amazon Web Services (AWS) Cloud. Using Amazon EC2 eliminates your need to invest in hardware up front, so you can develop and deploy applications faster. You can use Amazon EC2 to launch as many or as few virtual servers as you need, configure security and networking, and manage storage. Amazon EC2 enables you to scale up or down to handle changes in requirements or spikes in popularity, reducing your need to forecast traffic.

### Amazon's Virtual Private Cloud (VPC) Limit

The default limit to create a VPC in a region is 5. Chef Automate HA on AWS deployment creates two VPCs, each for the bastion host and for the rest of the node in a cluster.

{{< note >}}

You require a minimum of three node clusters for ElaticSearcg and Postgres-sql instances.

{{< /note >}}

!--  Do we need to cover about firewall settings? https://github.com/chef/a2ha-docs/wiki/FireWall-Setting

CIDR block —Classless Inter-Domain Routing. An internet protocol address allocation and route aggregation methodology. For more information, see Classless Inter-Domain Routing in Wikipedia.
Subnet — A range of IP addresses in your VPC.