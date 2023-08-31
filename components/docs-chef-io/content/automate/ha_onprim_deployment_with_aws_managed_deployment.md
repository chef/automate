+++
title = "On-premise Deployment with AWS Managed Database"
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "On-premise Deployment with AWS Managed Database"
    parent = "automate/deploy_high_availability/deployment"
    identifier = "automate/deploy_high_availability/deployment/ha_onprim_deployment_with_aws_managed_deployment.md On-premise Deployment with AWS Managed Database"
    weight = 210
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

This section will discuss deploying Chef Automate HA on-premise machines with AWS Managed Database. Please see the [On-Premises Prerequisites](/automate/ha_on_premises_deployment_prerequisites/) page and move ahead with the following sections of this page.

See the steps [here](/automate/ha_onprim_deployment_procedure/#steps-to-run-on-bastion-host-machine) to run on Bastion to download the latest Automate CLI and Airgapped Bundle.

## Steps to Generate Config

1. Generate config using the below command:

```bash
sudo chef-automate config gen config.toml
```

Click [here](/automate/ha_config_gen) to know more about generating config.

You can also view the [Sample Config](#sample-config-to-setup-on-premise-deployment-with-aws-managed-services).

{{< note >}} You can also generate config using **init config** and then generate init config for existing infrastructure. The command is as shown below:

`chef-automate init-config-ha existing_infra`{{< /note >}}

## Config Verify

1. We verify the above config using the below command:

    ```bash
    sudo chef-automate verify -c config.toml
    ```

    To know more about config verify you can check [Config Verify Doc page](/automate/ha_verification_check/).

    Once the verification is successfully completed, then proceed with deployment, In case of failure please fix the issue and re-run the verify command.

## Steps to Deploy

Continue with the deployment after generating the config:

```bash
   #Run commands as sudo.
   sudo -- sh -c "
   #Verify the data in the config
   cat config.toml
   #Run deploy command to deploy `automate.aib` with set `config.toml`
   chef-automate deploy config.toml --airgap-bundle automate.aib
   "
```

## Verify Deployment

Verify the deployment by checking status summary:

```bash
    sudo -- sh -c "
    #After Deployment is done successfully. Check the status of Chef Automate HA services
    chef-automate status summary
    "
```

Check if Chef Automate UI is accessible by going to (Domain used for Chef Automate) [https://chefautomate.example.com](https://chefautomate.example.com).

After successful deployment, proceed with following:

   1. Create user and orgs, Click [here](/automate/ha_node_bootstraping/#create-users-and-organization) to learn more about user and org creation
   1. Workstation setup, Click [here](/automate/ha_node_bootstraping/#workstation-setup) to learn more about workstation setup
   1. Node bootstrapping,  Click [here](/automate/ha_node_bootstraping/#bootstraping-a-node) to learn more about node bootstraping.

## Backup/Restore

A shared file system is always required to create OpenSearch snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. To know more about the backup and restore configuration, see On-Premise Deployment using [Filesystem](/automate/ha_backup_restore_file_system) or using [Object Storage](/automate/ha_backup_restore_object_storage).

## Add/Remove Nodes

The Chef Automate commands require some arguments so that it can determine which types of nodes you want to add or remove to/from your HA setup from your bastion host. To know more see [Add Nodes to the Deployment](/automate/ha_add_nodes_to_the_deployment) to add nodes and [Remove Single Node from Cluster](/automate/ha_remove_single_node_from_cluster) to remove nodes.

## Patch Configs

The bastion server can patch new configurations in all nodes. To know more see [Patch Configuration](/automate/ha_config/#patch-configuration) section.

## Sample Config to setup On-Premise Deployment with AWS Managed Services

```config
[architecture]
  [architecture.existing_infra]
    ssh_user = "ec2-user"
    ssh_group_name = "ec2-user"
    ssh_key_file = "~/.ssh/my-key.pem"
    ssh_port = "22"
    secrets_key_file = "/hab/a2_deploy_workspace/secrets.key"
    secrets_store_file = "/hab/a2_deploy_workspace/secrets.json"
    architecture = "existing_nodes"
    workspace_path = "/hab/a2_deploy_workspace"
    backup_mount = "/mnt/automate_backups"
    backup_config = "object_storage"
[object_storage]
  [object_storage.config]
    bucket_name = "fdjlfdsklfds"
    access_key = "CCAI..............."
    secret_key = "JVS................"
    endpoint = "https://s3.amazonaws.com"
    region = "us-east-2"
[automate]
  [automate.config]
    admin_password = "adminpassword"
    fqdn = "chefautomate.example.com"
    config_file = "configs/automate.toml"
    root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"
[chef_server]
  [chef_server.config]
    fqdn = "chefinfraserver.example.com"
    lb_root_ca = "-----BEGIN CERTIFICATE-----
    -----END CERTIFICATE-----"
    instance_count = "2"
[opensearch]
  [opensearch.config]
    instance_count = "0"
[postgresql]
  [postgresql.config]
    instance_count = "0"
[existing_infra]
  [existing_infra.config]
    automate_private_ips = ["192.0.0.1", "192.0.0.2"]
    chef_server_private_ips = ["192.0.0.3", "192.0.0.4"]
[external]
  [external.database]
    type = "aws"
    [external.database.postgre_sql]
      instance_url = "pg.aws.com:5432"
      superuser_username = "masteruser"
      superuser_password = "masterpassword"
      dbuser_username = "dbusername"
      dbuser_password = "dbpassword"
    [external.database.open_search]
      opensearch_domain_name = "opensearchdomain"
      opensearch_domain_url = "os.aws.com"
      opensearch_username = "osuser"
      opensearch_user_password = "opensearchpassowrd"
      [external.database.open_search.aws]
        aws_os_snapshot_role_arn = "arn:aws:acm:ap-southeast-2:112758395563:certificate/9b04-6513-4ac5-9332-2ce4e"
        os_snapshot_user_access_key_id = "CCAI..............."
        os_snapshot_user_access_key_secret = "JVS................"
```

## Uninstall Chef Automate HA

To uninstall Chef Automate HA instances after unsuccessful deployment, run the below command in your bastion host.

```bash
    chef-automate cleanup --onprem-deployment
```
