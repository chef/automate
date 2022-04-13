+++
title = "On-Premises Deployment Model"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-Premises Deployment Model"
    parent = "automate/deploy_high_availability/on_premises_deployment"
    identifier = "automate/deploy_high_availability/on_premises_deployment/ha_deploy_bareinfra.md On-Premises Deployment Model"
    weight = 200
+++

This section explains the Bare Metal (on-premises) Infrastructure Deployment (existing_node) to support Chef Automate High Availability (HA) in your network space.

## Pre-requisites

- Obtain necessary virtual machine (VM) instance details (with private IP addresses and added public address for Elasticsearch) to create the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch nodes.
- Obtain Bastion host server details from your system administrator. Ensure this server has the [needed requirements]({{< relref "ha_bastion.md#Bastion Host Requirements for On-Premise Deployment" >}}) included.
- Obtain the [Prerequisites for Chef Automate HA Deployment]({{< relref "ha_platform_support.md" >}}).
- All VMs must expose port 22 for SSH. You may need to open specific ports across the VMs to establish the communication, which are:

   | Component                                | Port                    |
   | :--------------------------------------  | :---------------------  |
   | Habitat gossip (UDP)                     |     9638                |
   | Habitat http API                         |     9631                |
   | PostgreSQL                               |     5432                |
   | Pgleaderchk                              |     6432                |
   | HaProxy                                  |     7432                |
   | Elasticsearch (https)                    |     9200                |
   | Elasticsearch (transport)                |     9300                |
   | Kibana                                   |     5601                |
   | Automate,ES-Node                         |     22,443              |

- Ensure you have [Chef Automate utility]({{< relref "ha_auto_install.md" >}}) installed, else download and install the latest version.
- Servers are provisioned and accessible through SSH from each other.
- Obtain load balancer IP addresses for all four instances - Chef Automate, Chef Server, Postgress, Elasticsearch.
- Create the certificates for security and authentication purposes. _optional_
- Rotate the certificates if the certificates are expired or compromised. _optional_
