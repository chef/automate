+++
title = "Configuration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Configuration"
    parent = "automate/deploy_high_availability/minimal_node_ha_deployment"
    identifier = "automate/deploy_high_availability/minimal_node_ha_deployment/ha_configuration.md Configuration and deployment"
    weight = 210
+++

Follow the steps below to deploy Chef Automate HA on-premise server or on existing nodes for minimal node ha deployment:

1. Open **Command Prompt**.
1. Log in as a **root** user by typing `sudo su -`.
1. Execute the command `./chef-automate init-config-ha existing_infra` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings.

1. Open the `config.toml` file in any editor and do the following:

   - Specify on-premise IPs, list of IP addresses for the cluster separated by a comma.
   - Here in total we have 5 nodes, 3 for backend and 2 for frontends  **For minimal node ha setup we need to specify same 3 node IPs for backend nodes that is postgresql and elasticsearch and for frontends also need to provide same 2 IPs for automate and chef-server**

   - Specify public IPs for the virtual machines. In case you do not have them, provide private IPs. The `config.toml` configuration file generates with default settings.
   - 

1. Execute the `cat config.toml` command and press **Enter** to view the generated configuration file.

1. Execute the `./chef-automate deploy config.toml` command and press **Enter**. This command creates deployment workspace (`/hab/a2_deploy_workspace`), downloads Habitat, and establishes cluster provisioning in your workspace.

{{< figure src="/images/automate/ha_bare_chef_automate_config.png" alt="Chef Automate Bare Infra Deployment">}}

1. Type `y` to confirm the terms of service and license agreement.

1. Log in as a root user by typing command, `sudo su`.

1. Execute the `cd /hab/a2_deploy_workspace` command and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

1. Make the following changes in `config.toml` file by opening the file in an editor. For example, `vi config.toml`.

   - Specify the `ssh username` and the `ssh key file path`. The ssh key must reside in the bastion host.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have the same value.
   - Assign permission to the **ssh key file** by running command, `chmod 400 /root/.ssh/id_rsa`.
   - Specify the number of nodes for the Chef Automate and Chef Infra server clusters. So the value for instance_count for automate and chef-server should be 2
   - Ensure not to modify the cluster number value as `3` for PostgreSQL and ElasticSearch.
   - Ensure the instance type supports the respective AWS region.

1. Setup the secret management key and the required passwords. The default location for the secrets key and secret storage is set in the _config.toml_ file. The default location for the key is `/etc/chef-automate/secrets.key`, and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

{{< figure src="/images/automate/minimal_node_config.png" alt="Chef Automate minimal node  `config.toml` file">}}

1. Execute the `./chef-automate deploy` command and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure. The deployment procedure creates the _HAB_ user by default.

{{< figure src="/images/automate/ha_bare_chef_automate_complete.png" alt="Chef Automate Bare Infra Deployment Confirmation">}}

1. Create a _uid_ or _gid_ for HAB user. Habitat automatically sets a uid and gid for the HAB user. You can override it if required or you can leave the field _habitat_uid_gid=""_ blank. _optional_

1. Execute the `./scripts/credentials set postgresql -auto` command and press **Enter**. This command rotates the credentials for Postgresql.

1. Execute the `./scripts/credentials set elasticsearch -auto` command and press **Enter**. This command rotates the credentials for ElasticSearch.

1. Execute the `chef-automate test -full` command and press **Enter**. This command runs smoke tests on the setup.

<!-- The default location for the secrets key and secret storage is set in the config file. The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json -->

## Clear the Bare Metal Infrastructure

{{< note >}}

You can clear the Bare-metal deployment workspace as per your requirements.

{{< /note >}}
