+++
title = "Configuration and Deployment"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Configuration and Deployment"
    parent = "automate/deploy_high_availability/minimal_node_ha_deployment"
    identifier = "automate/deploy_high_availability/minimal_node_ha_deployment/ha_configuration.md Configuration and Deployment"
    weight = 210
+++

In this section, we will talk about the basic configurations and deployment process of the Chef Automate HA on-premise server.

Follow the steps below to deploy Chef Automate HA on-premise server or on existing nodes for Minimal Node HA Deployment:

* Open **Command Prompt** and log in as a **root** user by typing `sudo su -` command.
* Execute the command `./chef-automate init-config-ha existing_infra` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings.
* Open the `config.toml` file in any editor and follow the steps below:

   - Specify on-premise IPs, list of IP addresses for the cluster separated by a comma.

   - Here in total we have five nodes, the for backend and two for frontends  **For Minimal Node HA setup specify same three node IPs for backend nodes that is Postgresql and Elasticsearch and for frontends also need to provide same 2 IPs for automate and chef-server**

   - Specify public IPs for the virtual machines. In case you do not have them, provide private IPs. The `config.toml` configuration file generates with default settings.

* Execute the `cat config.toml` command and press **Enter** to view the generated configuration file.

* Execute the `./chef-automate deploy config.toml` command and press **Enter**. This command creates deployment workspace (`/hab/a2_deploy_workspace`), downloads Habitat, and establishes cluster provisioning in your workspace.

{{< figure src="/images/automate/ha_bare_chef_automate_config.png" alt="Chef Automate Bare Infra Deployment">}}

* Type `y` to confirm the Terms of Service and License Agreement.

* Log in as a root user using `sudo su` command.

* Execute the `cd /hab/a2_deploy_workspace` command and press **Enter**. This command sets up the initial workspace directory and changes the working directory to Chef Automate workspace configured.

* Open the `config.toml` file in the editor using `vi config.toml` command. Follow the steps given below:

   - Specify the `ssh username` and the `ssh key file path`. The ssh key must reside in the bastion host.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have the same value.
   - Assign permission to the **ssh key file** by running `chmod 400 /root/.ssh/id_rsa` command.
   - Specify the number of nodes for the **Chef Automate** and **Chef Infra Server** clusters. The value for `instance_count` for automate and Chef Server will be two.
   - Ensure not to modify the cluster number value as `3` for **PostgreSQL** and **OpenSearch**.
   - Ensure the instance type supports the respective AWS region.

* Setup the secret management key and the required passwords. The default location for the secrets key and secret storage is set in the *config.toml* file. The default location for the key is `/etc/chef-automate/secrets.key`, and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

{{< figure src="/images/automate/minimal_node_config.png" alt="Chef Automate Minimal Node config file">}}

* Execute the `./chef-automate deploy` command and press **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure. The deployment procedure creates the *HAB* user by default.

{{< figure src="/images/automate/ha_bare_chef_automate_complete.png" alt="Chef Automate Bare Infra Deployment Confirmation">}}

* Create a **uid** or **gid** for HAB user. Habitat automatically sets a uid and gid for the HAB user. You can override it if required or you can leave the field *habitat_uid_gid=""* blank.

Some of the optional executions are as follows:

* Execute the `./scripts/credentials set postgresql -auto` command and press **Enter**. This command rotates the credentials for Postgresql.

* Execute the `./scripts/credentials set opensearch -auto` command and press **Enter**. This command rotates the credentials for OpenSearch.

* Execute the `chef-automate test -full` command and press **Enter**. This command runs smoke tests on the setup.

<!-- The default location for the secrets key and secret storage is set in the config file. The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json -->

## Clear the Bare Metal Infrastructure

{{< note >}}
You can clear the Bare-metal deployment workspace as per your requirements.
{{< /note >}}
