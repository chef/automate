+++
title = "On-Prem Deployment using Filesystem"

draft = false

gh_repo = "automate"

[menu]
    [menu.automate]
        title = "On-Prem Deployment using Filesystem"
        identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore File System"
        parent = "automate/deploy_high_availability/backup_and_restore"
        weight = 210
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

{{< note >}}

- If the user chooses `backup_config` as `file_system` in `config.toml` backup is already configured during the deployment, and in that case **the below steps are not required**. If `backup_config` is left blank, then the configuration needs to be configured manually.

{{< /note >}}

## Overview

A shared file system is always required to create **OpenSearch** snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location (or one of its parent directories) in the `path.repo` setting on all master and data nodes.

### Setting up the backup configuration

#### Configuration in OpenSearch Node

- Mount the shared file system to the base mount path which is mentioned in `backup_mount` on **all** OpenSearch and Frontend servers. 

{{< note >}}

- `/mnt/automate_backups` is the default value for the `backup_mount`, which is also used in this document page as reference backup path.

{{< /note >}}

Apply the following steps on **all of the OpenSearch server** node

- Create an OpenSearch sub-directory and set permissions (only if the network mount is correctly mounted).

    ```sh
    sudo mkdir /mnt/automate_backups/opensearch
    sudo chown hab:hab /mnt/automate_backups/opensearch/
    ```

#### Configuration for OpenSearch Node from Bastion Host

Configure the OpenSearch `path.repo` setting by following the steps given below:

- Create a .toml (say os_config.toml) file in **the Bastion host** and copy the following template with the path to the repo.

    ```sh
      [path]
      # Replace /mnt/automate_backups with the backup_mount config found on the Bastion host in /hab/a2_deploy_workspace/a2ha.rb
      repo = "/mnt/automate_backups/opensearch"
    ```

- Following command will add the configuration to the OpenSearch node.

    ```sh
      chef-automate config patch --opensearch <PATH TO OS_CONFIG.TOML>
    ```

##### Healthcheck commands

- Following command can be run in the OpenSearch node

    ```sh
    chef-automate status
    ```

- Following command can be run in the OpenSearch node

    ```sh
    hab svc status (check whether OpenSearch service is up or not)

    curl -k -X GET "<https://localhost:9200/_cat/indices/*?v=true&s=index&pretty>" -u admin:admin (Another way to check is to check whether all the indices are green or not)

    # Watch for a message about OpenSearch going from RED to GREEN
    journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
    ```

#### Configuration for Automate Node from Provision Host

- Configure Automate to handle _External OpenSearch Backups_.

- Create an `automate.toml` file on **the provisioning server** using the following command:

    ```bash
    touch automate.toml
    ```

    Add the following configuration to `automate.toml` on the provisioning host:

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

- Patch the `automate.toml` config to trigger the deployment from the provision host.

    ```sh
    chef-automate config patch --fe automate.toml
    ```

## Backup and Restore

### Backup

To create the backup, by running the backup command from bastion. The backup command is as shown below:

```sh
chef-automate backup create
```

### Restore

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

- Check the status of Automate HA Cluster from the bastion nodes by executing the `chef-automate status` command.

- Execute the restore command from bastion`chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --airgap-bundle </path/to/bundle>`.

{{< note >}}

- If you are restoring the backup from an older version, then you need to provide the `--airgap-bundle </path/to/current/bundle>`.

{{< /note >}}

#### Troubleshooting

{{< readfile file = "content/automate/reusable/md/restore_troubleshooting.md" >}}
