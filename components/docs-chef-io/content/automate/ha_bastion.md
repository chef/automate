+++
title = "Bastion Host"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Bastion Host"
    parent = "automate/deploy_high_availability/ha_system_requirements"
    identifier = "automate/deploy_high_availability/ha_system_requirements/ha_bastion.md Bastion Host"
    weight = 220
+++

A [Bastion Host](https://en.wikipedia.org/wiki/Bastion_host#:~:text=A%20bastion%20host%20is%20a,the%20threat%20to%20the%20computer.) is a special-purpose computer or server on a network specifically designed and configured to withstand attacks. This serve type generally hosts a single application or process, for example, a proxy server or load balancer. All other services are limited to reduce the threat to the computer.

It provides access to a private network from an external network, such as the internet or outside of a firewall, and involves access from untrusted networks or computers. These computers are also equipped with unique networking interfaces to withstand high-bandwidth attacks through the internet.

## Bastion Host for Chef Automate High Availability (HA)

The Virtual machine is required for either of the Chef Automate HA deployment types to trigger the deployment, actually a bastion host. This section explains the bastion host requirements and configurations for the two deployment modes of the Chef Automate HA.

### Bastion Host Prerequisites for On-Premise Deployment

- Bastion Server/host IP address
- Instance type: 2 vCPU
- Operating System: Ubuntu 18.04, 20.04/Centos 7/ RHEL 7, 8
- Memory: Minimum of 4GB
- Hard Disk Space: 100 GB
- Ports to be opened: 22 and 9631

Refer to [On-premises Deployment Model]({{< relref "ha_deploy_bareinfra.md" >}}) page for the deployment procedure.

### Bastion Host Prerequisites for AWS (Amazon Web Services) Deployment

- [AWS Credential configured on your bastion host]({{< relref "ha_configure_aws_credentials.md" >}}).
- Create the certificate for the DNS
- Operating System (OS): Ubuntu 18.04, 20.04/Centos 7/ RHEL 7, 8
- AWS instance type: *m5.large*
- Hard Disk Space - 100 GB
- Ports to be opened: 22 and 9631
- [Setup Virtual Private Cloud (VPC) in AWS]({{< relref "ha_vpc_setup.md" >}})
- This bastion host must be created in the same VPC.

Refer to [AWS Deployment Model]({{< relref "ha_deploy_aws.md" >}}) page for building a bastion host and Chef Automate HA deployment procedure using AWS.
