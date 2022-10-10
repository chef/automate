+++
title = "VPC and CIDR Setup"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "VPC and CIDR Setup"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_vpc_setup.md HA VPC and CIDR Setup"
    weight = 200
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This page explains the Amazon VPC and CIDR notations concepts. You need to provide these values in `config.toml` values to deploy Chef Automate High Availability (HA) using AWS.

## Understanding VPC

Amazon Virtual Private Cloud (VPC) enables you to launch AWS (Amazon Web Services) resources into your virtual network. This virtual network resembles a traditional network that you had to operate in your own data center, with the benefits of using the AWS scalable infrastructure.

Amazon VPC is the networking layer for Amazon EC2 (Elastic Compute Cloud). Amazon EC2 provides scalable computing capacity in the AWS Cloud. Using Amazon EC2 eliminates your need to invest in hardware upfront, so that you can develop and deploy applications faster. You can use Amazon EC2 to launch as many or a few virtual servers as you need, configure security and networking, and manage storage. Amazon EC2 enables you to scale up or down to handle changes in requirements or spikes in popularity, reducing your need to forecast network traffic.

VPC creates an isolated virtual network environment in the AWS cloud, dedicated to your AWS account. Other AWS resources and services operate inside VPC networks to provide cloud services. A VPC behaves like a traditional TCP/IP network in a physical data center that can be expanded and scaled. However, the Domain Controllers (DC) components used to deal with routers, switches, and VLANS do not explicitly exist in a VPC. They are abstracted and re-engineered into cloud software.

All VPCs are created and exist in one AWS region. AWS regions are geographic locations where Amazon clusters its cloud data centers. The advantage of regionalization is that a regional VPC provides network services originating from that geographical area. If you need to provide closer access for customers in another region, you can set up another VPC in that region. This aligns with the theory of AWS cloud computing, where IT applications and resources are delivered through the internet on-demand and with pay-as-you-go pricing. Limiting VPC configurations to specific regions allows you to selectively provide network services where they are needed, as they are needed.

Each Amazon account can host multiple VPCs. Because VPCs are isolated, you can duplicate private subnets among VPCs the same way you could use the same subnet in two different physical data centers. You can also add public IP addresses that can be used to reach VPC-launched instances from the internet.

You can modify or use that VPC for your cloud configurations, or build a new VPC and support services from scratch. However, no VPCs can communicate directly.

## Understanding CIDR

CIDR stands for Classless Inter-Domain Routing, a 32 -bit number underlying the octets. We need to understand IPv4 addresses/ notation to understand CIDR blocks.

For e.g., 10.10.101.5, might be the database's address, and it's a 32-bit binary number. The 10 maps to the first octet of the 0001010, another octet for the second 10, a third octet, fourth octet, each one ranging from 0 to 255 as far as our numbers. We are not describing a single number and a range of numbers, all the possible IP addresses that begin with the numbers 10.10. To describe a number, a range of numbers, that begin 10.10 using CIDR notation, we are freezing the first 16 bits and wild card the rest, so we draw out the number as 10.10, meaning these are the numbers that are going to stay the same. It doesn't matter commonly we'll put zeros here. And then, after the slash, how many bits are frozen, so 10.10.0.0. Whatever/16 indicates, in this case, the first 2 octets never change. The last 2 can be whatever you want. Inside a notation is determining how many bits you are freezing. The rest are all being wild carded in IPv4 notation.

/16 is the most common number we'll see for a VPC CIDR block. It's also the most you're allowed to do. You certainly could go smaller than /16, and smaller being 17, 18, 19, and so on in this case. You can go as small as /28, but that's going to give you a possible 12 addresses, 16 addresses minus the ones we take away, that's going to be for use inside your VPC, whereas a /16 is going to give you about 65,000 possible addresses.

If /16 is my CIDR block for your main VPC, we then subdivide into subnets, which need to be a subset of the /16. They all need to start at least with a 10.10, but if multiple subnets are required without any collisions, commonly we'll see those as /24. What a /24 means, means the first 24 bits are frozen. Say the subnet where the database lives, we call that out as 10.10.101. If /24 is defined, one could have any private IP address beginning from 0 to 255. In other words, wildcard the last 8 bits.

A /32 is a single specific address. In this case, 10.10.101.5, not wildcarding anything. Only one IP address is used to authorize a security group traffic.  Likewise, if you want to authorize traffic from the entire internet, wild card everything. 000/0 becomes the final notation to the front-end web servers or other elements.

## VPC Setup

You can either create a new VPC or use an existing available one in the region where you are setting up the Chef Automate HA infrastructure.

### VPC Limit

The default limit to create a VPC in a region is *5*. However, if the VPCs used in the respective region are exhausted, you can increase the limit in your AWS account. Chef Automate HA on AWS deployment creates two VPCs, one for the bastion host and another for the rest of the node in a cluster.

{{< note >}}

You require a minimum of three node clusters for ElaticSearcg and Postgres-sql instances.

{{< /note >}}

AWS limits the size of each VPC; a user cannot change the size once the VPC is created. Amazon VPC also sets a limit of **200 subnets per VPC**, which can support a minimum of **14 IP addresses**. AWS places further limitations per account or region, including limiting the number of VPCs to five, the number of Elastic IP addresses to five, the number of Internet gateways per VPC to one, the number of virtual private gateways to five, and the number of customer gateways to 50.

Classless interdomain routing (CIDR) IPv4 and IPv6 blocks defines VPC IP address ranges. You can add primary and secondary CIDR blocks to your VPC if the secondary CIDR block comes from the same address range as the primary block.

### Copying an Existing VPC

1. Navigate to the **AWS Management Console**.

1. Select your **AWS Region** from the top toolbar.

1. From the navigation pane, select **VPC Dashboard** in the upper-left corner.

1. Select **Your VPCs**.

1. Copy any available **VPC ID** from the **Your VPCs** screen.

{{< figure src="/images/automate/ha_aws_vpc_existing.png" alt="Using Existing VPC">}}

#### Adding a Private Subnet to the Available VPC

1. From the navigation pane, select **Subnets**.

1. Search your **VPC ID** in the *Subnets* screen.

1. Copy the corresponding **IPv4 CIDR** value.

{{< figure src="/images/automate/ha_aws_vpc_existing_subnet.png" alt="Using Existing VPC Subnet Value">}}

### Creating a VPC

1. Navigate to the **AWS Management Console**.

1. Select your **AWS Region** from the top toolbar.

1. From the navigation pane, select **VPC Dashboard** in the upper-left corner.

1. Select **Your VPCs**.

1. Select **Create VPC** from the left.

1. For **IPv4 CIDR** block, enter the CIDR block for the VPC. We recommend using a CIDR block from the private (non-publicly routable) IP address ranges. For example, 10.0.0.0/16. For more information, refer [VPC and Subnet Sizing for IPv4 page](https://docs.aws.amazon.com/vpc/latest/userguide/VPC_Subnets.html#vpc-sizing-ipv4).

1. For **IPv6 CIDR** block, keep No IPv6 CIDR Block.

1. For the  VPC name, enter a **tag name**.

1. Select **Create VPC**. After the VPC is created, choose **OK**.

{{< figure src="/images/automate/ha_aws_vpc.png" alt="VPC Creation">}}

#### Adding a Private Subnet

1. From the navigation pane, select **Subnets**.

1. Select **Create Subnet**.

1. Select your **VPC ID** from the drop-down.

1. Enter **Subnet name** for the private subnet (for example, WorkSpaces Private Subnet 2).

1. To make an appropriate selection for **Availability Zone**, see Availability Zones for Amazon WorkSpaces.

1. Enter the CIDR block for the subnet in the **IPv4 CIDR block**. For example, 10.0.2.0/24. Ensure you provide a unique value.

1. Select **Create Subnet**.

{{< figure src="/images/automate/ha_aws_subnet.png" alt="VPC Subnets">}}

{{< note >}}

Refer [select correct CIDR block](https://www.calculator.net/ip-subnet-calculator.html?cclass=a&csubnet=20&cip=172.31.0.0&ctype=ipv4&printit=0&x=82&y=36) page for selecting unique CIDR value for your VPC ID to make it unique.

{{< /note >}}

### Example config.toml file with VPC and CIDR Values

```ruby
region = "ap-south-1"
aws_vpc_id  = "vpc-8d1390e5"
aws_cidr_block_addr  = "172.31.128.0"
```
