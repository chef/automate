+++
title = "What is Bastion Host"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "What is Bastion Host"
    parent = "automate/install"
    identifier = "automate/install/ha_bastion.md What is HA Bastion Host"
    weight = 210
+++

## Bastion Host

A [Bastion Host](https://en.wikipedia.org/wiki/Bastion_host#:~:text=A%20bastion%20host%20is%20a,the%20threat%20to%20the%20computer.) is a special-purpose computer or server on a network specifically designed and configured to withstand attacks. This serve type generally hosts a single application or process, for example, a proxy server or load balancer, and all other services are limited to reduce the threat to the computer.

Its purpose is to provide access to a private network from an external network, such as the Internet or outside of a firewall and involves access from untrusted networks or computers. These computers are also equipped with special networking interfaces to withstand high-bandwidth attacks through the internet.

Bastion servers are instances that resides within your public subnet and accessed using SSH. The purpose of a bastion host is to restrict access to a private network from an external network. Once remote connectivity establishes with the bastion host, it allows you to use SSH to login to other instances (within private subnets) deeper within your network.

The bastion hosts provide secure access to Linux instances located in the private and public subnets.

## Bastion Host for Chef Automate High Availability (HA)

Virtual machine is required for either of the Chef Automate HA deployment types to trigger the deployment, which is actually a bastion host. This page explains the bastion host requirements and configurations for the two deployment modes of the Chef Automate HA.
