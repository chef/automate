+++
title = "Pre-Requisites"

draft = false

gh_repo = "Pre-Requisites"
[menu]
  [menu.automate]
    title = "Pre-Requisites"
    parent = "automate/deploy_high_availability"
    identifier = "automate/settings/chef-automate-ha-prerequisites.md Pre-Requisites"
    weight = 12
+++

Before installing Chef automate HA, ensure you have taken a quick tour of this pre-requisite page.

## Topology Based

### Hardware Requirements

Click [here](/automate/ha_platform_support/#hardware-requirements) to check with the hardware requirements before you start working on Chef Automate HA.

### Load Balancer

You can setup your load balancer using:

* [NGINX](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
* [HA Proxy](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)

### Software Requirements

Click [here](/automate/ha_platform_support/#software-requirements) to check for the mandatory operating system, virtual machine instances and VPC for implementing the Chef Automate High Availability (HA) in your network.

### Security Checks

HA cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. Click [here](/automate/ha_security_firewall/#ports-required-for-all-machines) for the breakdown of those ports and what needs to be open for each set of servers.

### Certificates


