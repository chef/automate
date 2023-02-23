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

The Automate HA supports three types of deployment:

* On-Premise Deployment
* AWS Deployment Deployment
* AWS Managed Services Deployment

The below requirements are elaborated according to the above three deployments.

## On-Premise Deployment Pre-Requisite

### Hardware Requirements

For the on-premise deployment, all the virtual machines should be up and running. The operating system root volume must be at least 40 GB with the TMP space of 5 GB. The number of nodes sending data should be of 5000 and the frequency of compliance scan, client runs and event feeds should be 1/hour each.

The separate Hab volume (/hab) provisioned should be at least 100 GB for OpenSearch node /hab volume will be more based on the data retention policy. Click [here](/automate/ha_platform_support/#hardware-requirements) to know about the hardware requirements and click [here](automate/ha_onprim_deployment_procedure/#prerequisites) to know more about the specifics of on-premise deployment pre-requisites.

Use the [Hardware Calculator](/calculator/automate_ha_hardware_calculator.xlsx) to check how much hardware you will need for your use-case.

### Load Balancer

LoadBalancers in on-premise deployment are set up according to [Chef Automate HA Architecture](/automate/ha/).

You can setup your load balancer using:

* [NGINX](/automate/loadbalancer_configuration/#load-balancer-setup-using-nginx)
* [HA Proxy](/automate/loadbalancer_configuration/#load-balancer-setup-using-ha-proxy)

### Software Requirements

The mandatory operating system for the om-premise deployment are Red Hat Enterprise Linux (64 Bit OS), Ubuntu (64 Bit OS), Centos (64 Bit OS), Amazon Linux 2 (64 Bit OS) and SUSE Linux Enterprise Server 12 SPS. Click [here](/automate/ha_platform_support/#software-requirements) to get the supported versions of the mentioned operating system.

While using any of the above operating system, the common user should have the sudo privileges and uses the same SSH Private Key file to access all machines. On_Premise deployment in Automate HA do not dupport passphrases for Private Key authentication.. Click [here](automate/ha_onprim_deployment_procedure/#prerequisites) to know more about the specifics of on-premise deployment pre-requisites.

### Security Checks

HA cluster requires multiple ports for the front and backend servers to operate effectively and reduce network traffic. Click [here](/automate/ha_security_firewall/#ports-required-for-all-machines) for the breakdown of those ports and what needs to be open for each set of servers.

Automate HA supports custom SSH port but the same port should be used accors all the machines.

### Chef Application Minimum Version


