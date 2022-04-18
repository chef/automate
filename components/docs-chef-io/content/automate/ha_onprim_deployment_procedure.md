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

In this section, we'll discuss about the steps to deploy Chef Automate HA on-premise server or on existing nodes. The steps are as follows:

1. Open **Command Prompt** and log in as a **root** user by typing `sudo su -`.
2. Run the command `./chef-automate init-config-ha existing_infra` and select **Enter** to set up the configuration for deployment. The `config.toml` configuration file gets generated with default settings.

3. Open the `config.toml` file in any editor and:

   - Specify the on-premise IPs list of IP addresses for the cluster separated by a comma.
   - Specify public IPs for the virtual machines. In case you do not have the public IPs, provide the private IPs. The `config.toml` configuration file gets generated with default settings.

4. Run the `cat config.toml` command and select **Enter** to view the generated configuration file.

5. Run the `./chef-automate deploy config.toml` command and select **Enter**. The command creates a deployment workspace (`/hab/a2_deploy_workspace`), downloads Habitat, and establishes cluster provisioning in your workspace.

{{< figure src="/images/automate/ha_bare_chef_automate_config.png" alt="Chef Automate Bare Infra Deployment">}}

6. Type `y` to confirm the terms of service and license agreement.

7. Log in as a root user using `sudo su` command.

8. Run the `cd /hab/a2_deploy_workspace` command and select **Enter**. This command sets up the initial workspace directory and changes the working directory to the Chef Automate workspace configured.

9. Open the `config.toml` file in the editor and execute the following changes:

   - Specify the `ssh username` and the `ssh key file path`. The ssh key must reside in the bastion host.
   - Ensure `ssh_key_pair_name` and `ssh key file path` have the same value.
   - Assign permission to the **ssh key file** by running the `chmod 400 /root/.ssh/id_rsa` command.
   - Specify the number of nodes for the Chef Automate and Chef Infra server clusters. By default, the deployment takes the value `1`.
   - Ensure not to modify the cluster number value as `3` for PostgreSQL and OpenSearch.
   - Ensure the instance type supports the respective AWS region.
   - Add *Load Balancer Certificate* details for Chef Automate and Chef Server as shown below:

<!-- automate_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e"
chef_server_lb_certificate_arn = "arn:aws:acm:ap-south-1:510367013858:certificate/1aae9fce-60df-4791-9bec-ef6a0f723f3e" -->

   {{< figure src="/images/automate/ha_bare_lb.png" alt="Load Balancer Details">}}

10. Setup the secret management key and the required passwords.

   - The default location for the secrets key and secret storage is set in the `config.toml` file.
   - The default location for the key is `/etc/chef-automate/secrets.key`.
   - The secret store file is in `/hab/a2_deploy_workspace/secrets.json`.

{{< figure src="/images/automate/ha_bare_chef_automate_configtoml_file.png" alt="Chef Automate Bare Infra `config.toml` file">}}

11. Run the `./chef-automate deploy` command and select **Enter**. This command installs the latest deployment package and deploys (by provisioning with terraform) airgap bundles on the created infrastructure. The deployment procedure by default creates the **HAB** user.

{{< figure src="/images/automate/ha_bare_chef_automate_complete.png" alt="Chef Automate Bare Infra Deployment Confirmation">}}

12. Create a **uid** or **gid** for HAB users. Habitat automatically sets a **uid** and **gid** for the HAB user. If required, you can override it or leave the field `_habitat_uid_gid=""_ blank`. (*Optional*)

13. Run the `./scripts/credentials set postgresql -auto` command and select **Enter**. This command rotates the credentials for Postgresql.

14. Run the `./scripts/credentials set opensearch -auto` command and select **Enter**. This command rotates the credentials for OpenSearch.

15. Run the `chef-automate test -full` command and select **Enter**. This command runs smoke tests on the setup.

<!-- The default location for the secrets key and secret storage is set in the config file. The default location for the key is /etc/chef-automate/secrets.key and the secret store file is in /hab/a2_deploy_workspace/secrets.json -->

## Clear the Bare Metal Infrastructure

{{< note >}} You can clear the Bare-metal deployment workspace as per your requirements. {{< /note >}}
