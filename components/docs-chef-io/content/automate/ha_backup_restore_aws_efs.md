+++
title = "AWS Deployment using EFS"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "AWS Deployment using EFS"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore AWS Deployment - EFS"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 230
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< note >}}

-   If the user chooses `backup_config` as `efs` in `config.toml` backup is already configured during deployment, **the below steps are not required and can be skipped**. i.e., **`backup_config = "efs"`** . If we have kept the `backup_config` blank, then the configuration needs to be configured manually.

{{< /note >}}

## Overview

A shared file system is always required to create **OpenSearch** snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location in the `path.repo` setting on all master and data nodes.

### Setting up the backup configuration

-   Create an EFS file system, please refer sample steps [here](https://docs.aws.amazon.com/efs/latest/ug/gs-step-two-create-efs-resources.html)

#### Configuration in OpenSearch Node

-   Mount the shared file system on all OpenSearch and Frontend servers:

    ```sh
    mount /mnt/automate_backups
    ```

-   Create an OpenSearch sub-directory and set permissions to one of the OpenSearch servers (only if the network mount is correctly mounted).

    ```sh
    sudo mkdir /mnt/automate_backups/opensearch
    sudo chown hab:hab /mnt/automate_backups/opensearch/
    ```

#### Configuration for OpenSearch Node from Provision host

Configure the OpenSearch `path.repo` setting by SSH to a single OpenSearch server by following the steps given below:

-   Create a .toml file (`os_config.toml`) and add the following settings to the end of the file.

    ```sh
    [path]
    # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in config.toml
    repo = "/mnt/automate_backups/opensearch"
    ```

-   To trigger the restart of the OpenSearch on each server, apply the updated `es_config.toml` config to OpenSearch once.

    ```sh
    chef-automate config patch --opensearch os_config.toml
    ```

#### Healthcheck commands

-   Following command can be run in the OpenSearch node

    ```sh
    hab svc status (check whether OpenSearch service is up or not)

    curl -k -X GET "<https://localhost:9200/_cat/indices/*?v=true&s=index&pretty>" -u admin:admin (Another way to check is to check whether all the indices are green or not)

    # Watch for a message about OpenSearch going from RED to GREEN
    `journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
    ```

#### Configuration for Automate node from Provision host

-   Create an `automate.toml` file on the provisioning server using the following command:

    ```bash
    touch automate.toml
    ```

-   Add the following configuration to `automate.toml` on the provisioning host:

    ```sh
    [global.v1.external.opensearch.backup]
    enable = true
    location = "fs"

    [global.v1.external.opensearch.backup.fs]
    # The `path.repo` setting you've configured on your OpenSearch nodes must be a parent directory of the setting you configure here:
    path = "/mnt/automate_backups/opensearch"

    [global.v1.backups.filesystem]
    path = "/mnt/automate_backups/backups"
    ```

-   Patch the `config` using below command.

    ```sh
    ./chef-automate config patch --frontend automate.toml
    ```

## Backup and Restore commands

### Backup

-   To create the backup, by running the backup command from bastion. The backup command is as shown below:

    ```cmd
    chef-automate backup create
    ```

### Restoring the EFS Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

-   Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

-   Execute the restore command from bastion`chef-automate backup restore <BACKUP-ID> -b /mnt/automate_backups/backups --airgap-bundle </path/to/bundle>`.

## Troubleshooting

While running the restore command, If it prompts any error follow the steps given below.

-  check the chef-automate status in Automate node by running `chef-automate status`.
-  Also check the hab svc status in automate node by running `hab svc status`.
-  If the deployment services is not healthy then reload it using `hab svc load chef/deployment-service`.
-  Now, check the status of Automate node and then try running the restore command from bastion.
