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

Now user can provide either public and private subnet ids or CIDR in config.toml

Steps for how to create VPC, public and private subnets :-


    1. Create a VPC.(choose CIDR like 10.0.0.0/16)
    2. Create public subnets under the above VPC
        2.1 Create 4 subnets in different Availability Zones (i.e us-west-2a, us-west-2b us-west-2c, for 4th subnet choose any of the 3 region).Choose cidr like (["10.0.0.0/20", "10.0.96.0/20", "10.0.16.0/20", "10.0.32.0/20"]

        2.2 Create 4 Route tables.

        2.3 Go to the route tables one by one and associate 1 subnet to each route table.

        2.4 Edit all the route of the table. Add one more route, select destination 0.0.0.0/0 and select the default internet gateway attached to the vpc.

    3. Create private subnets:-
3.1 create 3 subnets in different Availability Zones (i.e us-west-2a, us-west-2b us-west-2c and choose cidr like ("10.0.128.0/20", "10.0.144.0/20", "10.0.160.0/20").

3.2 create 3 route tables.

3.3 create 3 NAT gateways.
      While creating Nat gateways, select any public subnet you created above for      subnet field, connectivity type should be public and then click on allocate elastic ip button.

            3.4 Go to the route tables one by one and associate 1 subnet to each route table.
            
            3.5 Edit all the route of the table. Add one more route, select destination 0.0.0.0/0   and select the NAT gateway.


{{< figure src="/images/automate/subnet_creation.png" alt="subnet">}}


Or if you want to create from scratch then go to launch VPC wizard select the fields as shown below and click create VPC button.

Note for CIDR input:-  Make sure that main route table (which gets created by default while creating VPC) is having route to internet gateway because the public subnets created by provision infra will be associated to the main route table only. 
