+++
title = "On-Prem Deployment with Chef Managed Database"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-Prem Deployment with Chef Managed Database"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_onprim_deployment_procedure.md On-premise Deployment with Chef Managed Database"
    weight = 200
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

This document explains how to deploy Chef Automate HA on on-premises machines with Chef Managed Database.
Please see the [On-Premises Prerequisites](/automate/ha_on_premises_deployment_prerequisites/) page and move ahead with the following sections of this page.

{{< warning >}}

- Do not modify the workspace path. It should always be `/hab/a2_deploy_workspace`.
- We don't support AD managed users in nodes. We only support local Linux users.
- If you have configured a sudo password for the user, you must create an environment variable `sudo_password` and set the password as the variable's value. Example: `export sudo_password=<password>`. And then, run all sudo commands with the `sudo -E or --preserve-env` option. Example: `sudo -E ./chef-automate deploy config.toml --airgap-bundle automate.aib`. This is required for the `chef-automate` CLI to run the commands with sudo privileges. Please refer [this](/automate/ha_sudo_password/) for details.
- If SELinux is enabled, deployment with configure it to `permissive` (Usually in case of RHEL SELinux is enabled)
{{< /warning >}}

## Provisioning

Provision the other nodes in the high availability cluster before deploying the bastion host.

Make sure you have all resources either on existing infrastructure or on existing cloud infrastructure (AWS/Google Cloud Platform).

## Deploy the bastion host

1. Run the following commands to download the latest Automate CLI and airgapped bundle:

    ```bash
    sudo -- sh -c "
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip \
    | gunzip - > chef-automate && chmod +x chef-automate \
    | cp -f chef-automate /usr/bin/chef-automate
    curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate.aib
    "
    ```

    To download specific version bundle, replace `latest.aib` with Chef Automate version number. For example, `4.2.59.aib`.

    {{< note spaces=4 >}}

    Chef Automate bundles are available for 365 days from the release of a version. However, the milestone release bundles are available for download forever.

    {{< /note >}}

    {{< note >}} If the airgapped bastion host differs, transfer the bundle file (`latest.aib`) and Chef Automate CLI binary (`chef-automate`) to the airgapped bastion host using the `scp` command. {{< /note >}}

    After transferring the bundle file to the airgapped bastion host, run the following commands:

    ```bash
    sudo -- sh -c "
    #Move the Chef Automate CLI to `/usr/bin`.
    cp -f chef-automate /usr/bin/chef-automate
    "
    ```
    {{< /note >}}

## Generate Chef Automate configuration file

1. Generate config.

    ```bash
    sudo chef-automate config gen config.toml
    ```

Click [here](/automate/ha_config_gen) to know more about generating config.

You can also view the [Sample Config](#sample-config). You can also view the [Sample Config For 5 Node Cluster](#sample-config-for-5-nodes-cluster).

{{< note >}}

You can also generate a configuration file using the `init-config` subcommand.

`chef-automate init-config-ha existing_infra`

{{< /note >}}

## Config Verify

1. Verify the above config using the `verify` subcommand.

    ```bash
    sudo chef-automate verify -c config.toml
    ```

    To know more about config verify, check [Config Verify Documentation](/automate/ha_verification_check/).

    Once the verification completed successfully, proceed with the deployment. In case of failure, fix the issue and verify it by re-running the verify command.

## Steps to Deploy

1. The following command will run the deployment. The deploy command will first run the verify command internally, to skip a verification process during deploy command use `--skip-verify` flag

    ```bash
    chef-automate deploy config.toml --airgap-bundle automate.aib
    ```

   To skip verification during deployment, use `--skip-verify` flag
    ```bash
     chef-automate deploy config.toml --airgap-bundle automate.aib --skip-verify
    ```

## Verify Deployment

1. Once the deployment is successful, get the consolidated status of the cluster

    ```bash
     chef-automate status summary
    ```

1.  Get the service status from each node

    ```bash
     chef-automate status
    ```

1. Post Deployment, you can run the verification command

    ```bash
     chef-automate verify
    ```

1. Get the cluster Info

    ```bash
     chef-automate info
    ```

    Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).
    After successful deployment, proceed with following...
      1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
      1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
      1. Node bootstrapping, Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstrapping.

## Backup/Restore

A shared file system is always required to create OpenSearch snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. To know more about the backup and restore configuration, see On-Premise Deployment using [Filesystem](/automate/ha_backup_restore_file_system) or using [Object Storage](/automate/ha_backup_restore_object_storage).

## Add/Remove Nodes

The Chef Automate commands require some arguments so that it can determine which types of nodes you want to add or remove to/from your HA setup from your bastion host. To know more, see [Add Nodes to the Deployment](/automate/ha_add_nodes_to_the_deployment) to add nodes and [Remove Single Node from Cluster](/automate/ha_remove_single_node_from_cluster) to remove nodes.

## Patch Configs

The bastion server can patch new configurations in all nodes. To know more see [Patch Configuration](/automate/ha_config/#patch-configuration) section.

## Sample Config

{{< note >}}

- Assuming 10+1 nodes (1 bastion, 2 for Chef Automate, 2 for Chef Infra Server, 3 for PostgreSQL, 3 for OpenSearch).
- The following config will, by default, leave the backup configuration empty.
- To provide multiline certificates use triple quotes like `""" multiline certificate contents"""`.

{{< /note >}}

```toml
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "/home/ec2-user/KEY_FILENAME.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "file_system"
[automate]
  [automate.config]
    admin_password = "Progress@123"
    fqdn = "chefautomate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    <Certificates>
    -----END CERTIFICATE-----"
    instance_count = "2"
[chef_server]
  [chef_server.config]
    fqdn = "chefinfraserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    <Certificates>
    -----END CERTIFICATE-----"
    instance_count = "2"
[opensearch]
  [opensearch.config]
    instance_count = "3"
[postgresql]
  [postgresql.config]
    instance_count = "3"
[existing_infra]
  [existing_infra.config]
    automate_private_ips = ["192.0.0.1", "192.0.0.2"]
    chef_server_private_ips = ["192.0.0.3", "192.0.0.4"]
    opensearch_private_ips = ["192.0.0.5", "192.0.0.6", "192.0.0.7"]
    postgresql_private_ips = ["192.0.0.8", "192.0.0.9", "192.0.0.10"]
```

## Sample Config For 5 Nodes Cluster

{{< note >}}

- Assuming 5+1 nodes (1 bastion, 2 for Chef Automate and Chef Infra Server, 3 for PostgreSQL and OpenSearch).
- For the Frontend nodes you can use the same IP in automate and chefserver.
- For the Backend nodes you can use the same IP in postgresql and opensearch.
- To provide multiline certificates use triple quotes like `""" multiline certificate contents"""`.

{{< /note >}}

```config
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "/home/ec2-user/my-key.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "file_system"
[automate]
  [automate.config]
    admin_password = "Progress@123"
    fqdn = "chefautomate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    <Certificates>
    -----END CERTIFICATE-----"
    instance_count = "2"
[chef_server]
  [chef_server.config]
    fqdn = "chefinfraserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    <Certificates>
    -----END CERTIFICATE-----"
    instance_count = "2"
[opensearch]
  [opensearch.config]
    instance_count = "3"
[postgresql]
  [postgresql.config]
    instance_count = "3"
[existing_infra]
  [existing_infra.config]
    automate_private_ips = ["192.0.0.1", "192.0.0.2"]
    chef_server_private_ips = ["192.0.0.1", "192.0.0.2]
    opensearch_private_ips = ["192.0.0.5", "192.0.0.6", "192.0.0.7"]
    postgresql_private_ips = ["192.0.0.5", "192.0.0.6", "192.0.0.7]
```

## Uninstall Chef Automate HA

To uninstall Chef Automate HA instances after unsuccessful deployment, run the `cleanup` command on your bastion host.

```bash
chef-automate cleanup --onprem-deployment
```
