+++
title = "Subnet Creation"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Subnet Creation"
    parent = "automate/deploy_high_availability/reference"
    identifier = "automate/deploy_high_availability/reference/ha_subnet_creation.md Subnet Creation"
    weight = 250
+++

Chef Automate HA Deployment provides you public and private subnet or CIDR in `config.toml` file. The steps to create the VPC of public and private subnets is given below:

- Create VPC (choose CIDR like **10.0.0.0/16**).
- Create public subnets under the above VPC:

    - Create four subnets in different Availability Zones (i.e us-west-2a, us-west-2b us-west-2c, for 4th subnet choose any of the 3 region).Choose **CIDR** ("10.0.0.0/20", "10.0.96.0/20", "10.0.16.0/20", "10.0.32.0/20").
    - Create four Route tables.
    - Go to the route tables one by one and associate 1 subnet to each route table.
    - Edit all the route of the table. Add one more route, select destination *0.0.0.0/0* and select the default internet gateway attached to the VPC.

- Create private subnets such as:

    - Create three subnets in different Availability Zones (i.e us-west-2a, us-west-2b us-west-2c and choose cidr like ("10.0.128.0/20", "10.0.144.0/20", "10.0.160.0/20").
    - Create three route tables.
    - Create three NAT gateways.

      The public subnet was created while creating the Nat gateways. Select a public subnet when creating the Nat gateways. The connectivity type for the same will be public. Once done select **Allocate Elastic IP** button.

    - Go to the route tables one by one and associate 1 subnet to each route table.
    - Edit all the route of the table and add one more route to it. Select destination *0.0.0.0/0* and select the *NAT gateway*.

{{< figure src="/images/automate/subnet_creation.png" alt="subnet">}}

To create subnet from scratch, using launch VPC wizard select the fields as shown below and select **Create VPC** button.

{{< note >}} Make sure the main route table (which gets created by default while creating VPC) is having route to internet gateway as the public subnets created by provision infra will be associated to the main route table only. {{< /note >}}
