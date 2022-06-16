+++
title = "High Availability Architecture"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Architecture"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha.md High Availability Architecture"
    weight = 10
+++

**High availability (HA)** refers to a system or application that offers a high level of operational availability. This means that if one server goes down, whether due to traffic overload or other issues, the entire site or application will not be down. HA means the application remains available with no interruption. We achieve high availability when an application continues to operate even when one or more underlying components fail.

Thus, HA is designed to avoid loss of service by reducing or managing failures and minimizing unscheduled downtime (when your system or network is not available for use or is unresponsive) that happens due to power outages or failure of a component.

## Chef Automate High Availability (HA)

The Chef Automate HA equates to reliability, efficiency, and productivity. Chef Automate HA is built on the following characteristics, **Redundancy**, and **Failover**. It aids in addressing major issues like service failure, and zone failure.

## Chef Automate HA Architecture

This architecture includes the cluster of the *Chef Automate*, *Chef Server*, *Postgres*, and *OpenSearch*.

### Chef Automate HA Architecture for OnPremise / AWS Non-Managed

![High Availability Architecture](/images/automate/ha_arch_onpremise.png)

### Chef Automate HA Architecture for AWS Managed

![High Availability Architecture](/images/automate/ha_arch_aws_managed.png)

## Chef Automate HA Topology

The Chef Automate HA Architecture involves the following clusters part of the main cluster:

- **Backend Cluster** (Persistant Services)
  - **Postgres:** Database requires a minimum of three nodes. Postgres database uses *Leader-Follower* strategy, where one becomes a leader, and the other two are the followers.

  - **OpenSearch:** Database requires a minimum of three nodes. OpenSearch database manages the [cluster internally](https://opensearch.org/docs/latest/opensearch/cluster/).

- **Frontend Cluster** (Application Services)
  - [Chef Automate](https://docs.chef.io/automate/)
  - [Chef Server](https://docs.chef.io/server/)

## Deployment Methods

Chef Automate High Availability (HA) supports two types of deployment:

- [On-premise Deployment (Existing Node) Deployment](/automate/ha_deploy_bareinfra)
- [Amazon Web Services (AWS) Deployment](/automate/ha_auto_install)

### On-premise Deployment (Existing Node/Bare Infrastructure)

In this we expect VM (Virtual machine) or Bare Metal machines (Physical machine) are already created and having initial Operating System (OS) setup done. Including Ports and Security policies changed according to requirements.

After this, installation steps will Deploy Chef Automate, Chef Infra Server, Postgresql DB and Opensearch DB to the relevant VMs or Physical Machines as provided in Config.

### Cloud Deployment using Amazon Web Services (AWS)

The two step deployment process is as shown below:

- Provisioning Infrastructure. (Optional, if already manually done)
- Deployment of services on provisioned infrastructure.
  - Installation of *PostgreSQL*, *OpenSearch*, *Chef Automate*, *Chef Infra Server* will be done in this step.
