+++
title = "HA Deployment Procedure for Bare Metal Infrastructure"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "HA Deployment Procedure for Bare Metal Infrastructure"
    parent = "automate/install"
    identifier = "automate/install/ha_deploy_bareinfra.md HA Deployment Procedure for Bare Metal Infrastructure"
    weight = 210
+++

This section explains the Bare Metal Infrastructure Deployment (existing_node) to support Chef Automate High Availability (HA) in your network premises/ infrastructure.

## Pre-requisites

- Chef Automate Utility.
- Servers provisioned and accessible through SSH from each other.
- List of Virtual Machines (VM) with public and private numbers. Public IP address is not mandatory.
- Obtain the IP address of the instances.
- Obtain load balancer IP addresses for all four instances - Chef Automate, Chef Server, Postgress, Elasticsearch
- Bastion Host. See [Bastion Host Requirements for Bare Infra Deployment method]({{< relref "ha_deploy_bareinfra.md#Bastion_Server_Requirements_for_Bare_Infra Deployment_Method" >}}) section.
- All VMs must expose the port 22 for SSH. You may need to open certain port across the VMs to establish the communication, which are:

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

### Download and Install Chef Automate Utility

Both types of deployment models require you to install and configure Chef Automate on your network infrastructure. You can skip this section if you already have installed the Chef Automate utility where you are planning to deploy HA.

Follow these steps to install **Chef Automate** utility on the fresh server.

- Open **Command Prompt** and navigate to your preferred location.
- Type the `curl` and `gunzip` commands together, `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate | cp -f chef-automate /usr/bin/chef-automate` and press **Enter**. The command downloads the latest version of the Chef Automate utility installer in .zip format.
- Type the  command,  and press **Enter**. The command installs the utility and provides the execute permission to the Chef Automate file.

  The installation of the Chef Automate utility completes and a confirmation message displays on your terminal as shown in the below screen.

{{< figure src="/images/automate/ha_bare_chef_automate_install.png" alt="Chef Automate Utility Installation">}}

## Deployment Procedure on Bare Infrastructure

Follow the steps below to deploy Chef Automate HA on-premise server or on existing nodes:

1. Open **Command Prompt**.
1. Login as a **root** user by typing `sudo su -`.
1. Type the command, `./chef-automate init-config-ha existing_infra` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings.

1. Open the `config.toml` file in any editor and do the following:

   1. Specify on-premise IPs, list of IP address for the cluster separated by comma.

   1. Specify public IPs for the virtual machines. In case, you dont have them, provide private IPs. The `config.toml` configuration file generates with default settings.

1. Type the command, `cat config.toml` and press **Enter** to view the generated configuration file.

1. Type the command, `./chef-automate deploy config.toml` and press **Enter**. This command creates deployment workspace (*/hab/a2_deploy_workspace*), downloads Habitat, and establish cluster provisioning in your workspace.

{{< figure src="/images/automate/ha_bare_chef_automate_config.png" alt="Chef Automate Bare Infra Deployment">}}

1. Type `y` to confirm the terms of service and license agreement.

1. Login as a root user by typing command, `sudo su`.

1. Type the command, `cd /hab/a2_deploy_workspace` and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

1. Make the following changes in `config.toml` file by opening the file in a editor. For example, `vi config.toml`.

   1. Specify the `ssh username` and the `ssh key file path`. The ssh key must reside in bastion host.
   1. Ensure `ssh_key_pair_name` and `ssh key file path` have same value.
   1. Assign permission to the *ssh key file* by running command, `chmod 400 /root/.ssh/id_rsa`.
   1. Specify the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes value `1`. 
   1. Ensure not to modify the cluster number value as `3` for PostgreSQL and ElasticSearch.
   1. Ensure the instance type supports the respective AWS region.
   1. Add load balancer certificate details for automate and chef-server. For example, as shown below:
   
      <!-- automate_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e"
      <!-- chef_server_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e" --> -->

      {{< figure src="/images/automate/ha_bare_lb.png" alt="Load Balancer Details">}}

   1. Setup the secrets management key and the required passwords. The default location for the secrets key and secret storage is set in the *config.toml* file. The default location for the key is `/etc/chef-automate/secrets.key` and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

{{< figure src="/images/automate/ha_bare_chef_automate_configtoml_file.png" alt="Chef Automate Bare Infra `config.toml` file">}}

1. Type the command, `./chef-automate secrets init` and press **Enter**. This command generates a new secrets key, which is used to encrypt the secret store. _optional_

1. Type the command, `./chef-automate secrets set automate_admin_password` and press **Enter**. This command sets up the Chef Automate User interface (UI) password. _optional_

1. Type the command `automate-cluster-ctl secrets set sudo_password` and press **Enter**. This command sets up the password for sudo. _optional_

1. Similarly, type the commands, `automate-cluster-ctl secrets set fe_sudo_password` and `automate-cluster-ctl secrets set be_sudo_password` and press **Enter**. These commands sets up the passwords for front end and back end nodes. _optional_

1. Type the command, `./chef-automate deploy` and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure. The deployment procedure creates the *HAB* user by default.

{{< figure src="/images/automate/ha_bare_chef_automate_complete.png" alt="Chef Automate Bare Infra Deployment Confirmation">}}

1. Create a *uid* or *gid* for HAB user. Habitat automatically sets a uid and gid for the HAB user. You can override it if required or you can leave the field *habitat_uid_gid=""* blank. _optional_

1. Type the command, `./scripts/credentials set postgresql -auto` and press **Enter**. This command rotates the credentials for Postgresql.

1. Type the command, `./scripts/credentials set elasticsearch -auto` and press **Enter**. This command rotates the credentials for ElasticSearch.

1. Type the command, `chef-automate test -full` and press **Enter**. This command runs smoke tests on the setup. 

<!-- The default location for the secrets key and secret storage is set in the config file. The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json -->

## Clear the Bare Metal Infrastructure

{{< note >}}

You can clear the Bare metal deployment workspace as per your requirements.

{{< /note >}}
