+++
title = "Deployment Procedure"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Deployment Procedure"
    parent = "automate/deploy_high_availability/on_premises_deployment"
    identifier = "automate/deploy_high_availability/on_premises_deployment/ha_onprim_deployment_procedure.md Deployment Procedure"
    weight = 220
+++

Follow the steps below to deploy Chef Automate HA on-premise server or on existing nodes:

1. Open **Command Prompt**.
1. Log in as a **root** user by typing `sudo su -`.
1. Execute the command `./chef-automate init-config-ha existing_infra` and press **Enter** to setup the configuration for deployment. The `config.toml` configuration file generates with default settings.

1. Open the `config.toml` file in any editor and do the following:

   - Specify on-premise IPs, list of IP addresses for the cluster separated by a comma.

   - Specify public IPs for the virtual machines. In case you do not have them, provide private IPs. The `config.toml` configuration file generates with default settings.

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
   - Specify the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment take the value `1`.
   - Ensure not to modify the cluster number value as `3` for PostgreSQL and ElasticSearch.
   - Ensure the instance type supports the respective AWS region.
   - Add load balancer certificate details for Chef Automate and Chef Server. For example, as shown below:

<!-- automate_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e"
chef_server_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e" -->

   {{< figure src="/images/automate/ha_bare_lb.png" alt="Load Balancer Details">}}

1. Setup the secret management key and the required passwords. The default location for the secrets key and secret storage is set in the _config.toml_ file. The default location for the key is `/etc/chef-automate/secrets.key`, and the secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

{{< figure src="/images/automate/ha_bare_chef_automate_configtoml_file.png" alt="Chef Automate Bare Infra `config.toml` file">}}

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
