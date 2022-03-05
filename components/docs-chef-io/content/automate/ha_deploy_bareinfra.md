+++
title = "On-premises Deployment"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-premises Deployment"
    parent = "automate/install/ha"
    identifier = "automate/install/ha_deploy_bareinfra.md On-premises Deployment"
    weight = 60
+++

This section explains the Bare Metal Infrastructure Deployment (existing_node) to support Chef Automate High Availability (HA) in your network premises/ infrastructure.

## Pre-requisites

- Obtain necessary virtual machine (VM) instance details (with private IP addresses and added public address for Elasticsearch) to create the cluster of the Chef Automate, Chef Server, Postgres, and Elasticsearch nodes.
- Obtain Bastion host server details from your system administrator. Ensure this server has the [needed requirements](( {{< relref "ha_bastion.md#Bastion Host Requirements for On-Premise Deployment" >}} )) included.
- Obtain the [Prerequisites for Chef Automate HA Deployment](( {{< relref "ha_architecture_reference.md#System Requirements" >}} ))..
- All VMs must expose port 22 for SSH. You may need to open certain ports across the VMs to establish the communication, which are:

   | Component                                | Port                    |
   | :--------------------------------------: | :---------------------: |
   | Habitat gossip (UDP)                     |     9638                |
   | Habitat http API                         |     9631                |
   | PostgreSQL                               |     5432                |
   | Pgleaderchk                              |     6432                |
   | HaProxy                                  |     7432                |
   | Elasticsearch (https)                    |     9200                |
   | Elasticsearch (transport)                |     9300                |
   | Kibana                                   |     5601                |
   | Automate,ES-Node                         |     22,443              |

- Ensure you have [Chef Automate utility](( {{< relref "ha_bastion.md#Download and Install the Chef Automate Utility" >}})) installed, else download and install the latest version.
- Servers provisioned and accessible through SSH from each other.
- Obtain load balancer IP addresses for all four instances - Chef Automate, Chef Server, Postgress, Elasticsearch.
- Create the certificates for security and authentication purposes. _optional_
- Rotate the certificates if the certificates are expired or compromised. _optional_

## Deployment Procedure on Bare Infrastructure

Follow the steps below to deploy Chef Automate HA on-premise server or on existing nodes:

1. Open **Command Prompt**.
1. Login as a **root** user by typing `sudo su -`.
1. Type the command, `./chef-automate init-config-ha existing_infra` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings.

1. Open the `config.toml` file in any editor and do the following:

   - Specify on-premise IPs, list of IP addresses for the cluster separated by comma.

   - Specify public IPs for the virtual machines. In case you do not have them, provide private IPs. The `config.toml` configuration file generates with default settings.

1. Type the command, `cat config.toml` and press **Enter** to view the generated configuration file.

1. Type the command, `./chef-automate deploy config.toml` and press **Enter**. This command creates deployment workspace (`/hab/a2_deploy_workspace`), downloads Habitat, and establishes cluster provisioning in your workspace.

{{< figure src="/images/automate/ha_bare_chef_automate_config.png" alt="Chef Automate Bare Infra Deployment">}}

1. Type `y` to confirm the terms of service and license agreement.

1. Login as a root user by typing command, `sudo su`.

1. Type the command, `cd /hab/a2_deploy_workspace` and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

1. Make the following changes in `config.toml` file by opening the file in an editor. For example, `vi config.toml`.

   - Specify the `ssh username` and the `ssh key file path`. The ssh key must reside in bastion host.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have same value.
   - Assign permission to the **ssh key file** by running command, `chmod 400 /root/.ssh/id_rsa`.
   - Specify the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes value `1`.
   - Ensure not to modify the cluster number value as `3` for PostgreSQL and ElasticSearch.
   - Ensure the instance type supports the respective AWS region.
   - Add load balancer certificate details for automate and chef-server. For example, as shown below:

   {{< figure src="/images/automate/ha_bare_lb.png" alt="Load Balancer Details">}}

   - Setup the secrets management key and the required passwords. The default location for the secrets key and secret storage is set in the **config.toml** file. The default location for the key is `/etc/chef-automate/secrets.key` and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

   <!-- automate_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e"
   chef_server_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e" -->

   {{< figure src="/images/automate/ha_bare_chef_automate_configtoml_file.png" alt="Chef Automate Bare Infra `config.toml` file">}}

1. Type the command, `./chef-automate deploy` and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure. The deployment procedure creates the **HAB** user by default.

{{< figure src="/images/automate/ha_bare_chef_automate_complete.png" alt="Chef Automate Bare Infra Deployment Confirmation">}}

1. Create a **uid** or **gid** for HAB user. Habitat automatically sets a uid and gid for the HAB user. You can override it if required or you can leave the field **habitat_uid_gid=""** blank. _optional_

1. Type the command, `./scripts/credentials set postgresql -auto` and press **Enter**. This command rotates the credentials for Postgresql.

1. Type the command, `./scripts/credentials set elasticsearch -auto` and press **Enter**. This command rotates the credentials for ElasticSearch.

1. Type the command, `chef-automate test -full` and press **Enter**. This command runs smoke tests on the setup.

<!-- The default location for the secrets key and secret storage is set in the config file. The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json -->

## Clear the Bare Metal Infrastructure

{{< note >}}

You can clear the Bare-metal deployment workspace as per your requirements.

{{< /note >}}
