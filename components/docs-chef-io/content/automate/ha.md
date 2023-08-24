+++
title = "High Availability Overview"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Overview"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/ha.md High Availability Overview"
    weight = 10
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

**High availability (HA)** refers to a system or application that offers a high level of operational availability. This means that the entire site or application will not be down if one server goes down due to traffic overload or other issues. HA represents the application remains available with no interruption. We achieve high availability when an application continues to operate even when one or more underlying components fail.

Thus, HA is designed to avoid loss of service by reducing or managing failures and minimizing unscheduled downtime (when your system or network is not available for use or is unresponsive) that happens due to power outages or failure of a component.

## Chef Automate High Availability (HA)

The Chef Automate HA equates to reliability, efficiency, and productivity, built on **Redundancy** and **Failover**. It aids in addressing significant issues like service failure and zone failure.

## Chef Automate HA Architecture

HA architecture includes the cluster of the *Chef Automate*, *Chef Server*, *Postgres*, and *OpenSearch*.

### Chef Automate HA Architecture for OnPremise / AWS Non-Managed

![High Availability Architecture](/images/automate/ha_arch_onprem.png)

### Chef Automate HA Architecture for AWS Managed

![High Availability Architecture](/images/automate/ha_arch_aws_managedservices.png)

{{< note >}}
Chef Automate HA for Managed Services has default port 7392 for Managed Postgresql and 9200 for Managed Opensearch. You can also change to your custom port.
{{< /note >}}
### Chef Automate HA Architecture for OnPremise Non-Managed Minimum Node Cluster

![High Availability Architecture](/images/automate/ha_arch_minnode_cluster.png)

## Chef Automate HA Topology

The Chef Automate HA Architecture involves the following clusters as part of the main cluster:

- **Backend Cluster** (Persistent Services)
  - **Postgres:** Database requires a minimum of three nodes. Postgres database uses the *Leader-Follower* strategy, where one becomes a leader, and the other two are the followers.

  - **OpenSearch:** Database requires a minimum of three nodes. OpenSearch database manages the [cluster internally](https://opensearch.org/docs/latest/opensearch/cluster/).

- **Frontend Cluster** (Application Services)
  - [Chef Automate](https://docs.chef.io/automate/)
  - [Chef Server](https://docs.chef.io/server/)

## Deployment Methods

Chef Automate High Availability (HA) supports two types of deployment:

- [On-premise Deployment (Existing Node) Deployment](/automate/ha_onprim_deployment_procedure/)
- [Amazon Web Services (AWS) Deployment](/automate/ha_aws_deploy_steps/)

### On-premise Deployment (Existing Node/Bare Infrastructure)

In this, we expect VM (Virtual machine) or Bare Metal machines (Physical machine) that are already created and have initial Operating System (OS) setup done. Including Ports and Security policies changed according to requirements.

After this, installation steps will Deploy Chef Automate, Chef Infra Server, Postgresql DB, and OpenSearch DB to the relevant VMs or Physical Machines as provided in Config.

Please refer [Performace Bench marking](https://docs.chef.io/automate/ha_performance_benchmarks/#performance-benchmarks) for more info.

### Cloud Deployment using Amazon Web Services (AWS)

The two-step deployment process is as shown below:

- Provisioning Infrastructure. (Optional, if already manually done)
- Deployment of services on the provisioned infrastructure.
  - Installation of *PostgreSQL*, *OpenSearch*, *Chef Automate*, and *Chef Infra Server* will be done in this step.
