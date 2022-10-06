+++
title = "On-Permise Deployment using Filesystem"

draft = false

gh_repo = "automate"

[menu]
    [menu.automate]
        title = "On-Permise Deployment using Filesystem"
        identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore File System"
        parent = "automate/deploy_high_availability/backup_and_restore"
        weight = 210
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

{{< note >}}

- If user choose `backup_config` as `file_system` in `config.toml,` backup is already configured during the deployment, and in that case **the below steps are not required**. If `backup_config` left blank, then configuration needs to be configure manually.

{{< /note >}}

## Overview

A shared file system is always required to create **OpenSearch** snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location (or one of its parent directories) in the `path.repo` setting on all master and data nodes.

### Setting up backup configuration

#### Configuration in Opensearch Node

- Mount the shared file system on **all** OpenSearch and Frontend servers :

    ```sh
    mount /mnt/automate_backups
    ```

{{< note >}}

- `/mnt/automate_backups` is the default value for the as a backup path, we can change to any other value.

{{< /note >}}

Apply following steps on **all of the OpenSearch server** node

- Create an OpenSearch sub-directory and set permissions (only if the network mount is correctly mounted).

    ```sh
    sudo mkdir /mnt/automate_backups/opensearch
    sudo chown hab:hab /mnt/automate_backups/opensearch/
    ```

Configure the OpenSearch `path.repo` setting by following the steps given below:

- Export the current OpenSearch config from the Habitat supervisor. Get the root access to run the following commands:

    ```sh
    source /hab/sup/default/SystemdEnvironmentFile.sh
    automate-backend-ctl applied --svc=automate-ha-opensearch | tail -n +2 > es_config.toml
    ```

- Edit `es_config.toml` and add the following settings to the end of the file.

    {{< note >}} If the credentials have never been rotated, the above file may be empty. {{< /note >}}

    ```sh
      [path]
      # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in /hab/a2_deploy_workspace/a2ha.rb
      repo = "/mnt/automate_backups/opensearch"
    ```

- The following command will apply the updated `es_config.toml` config to all the OpenSearch nodes and will trigger restart of opensearch in all nodes.
  Execution of the below command is any one of the opensearch node.

    ```sh
    hab config apply automate-ha-opensearch.default $(date '+%s') es_config.toml
    ```

##### Healthcheck commands

    ```sh
    hab svc status (check whether OpenSearch service is up or not)

    curl -k -X GET "<https://localhost:9200/_cat/indices/*?v=true&s=index&pretty>" -u admin:admin (Another way to check is to check whether all the indices are green or not)

    # Watch for a message about OpenSearch going from RED to GREEN
    `journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
    ```

##### Configuration in Provision host

- Configure Automate to handle _External OpenSearch Backups_.

- Create an `automate.toml` file on the provisioning server using the following command:

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

- Patch the `automate.toml` config to trigger the deployment from provision host.

    ```sh
    ./chef-automate config patch automate.toml
    ```

## Backup and Restore commands

### Backup

To create the backup, by running the backup command from a Chef Automate front-end node. The backup command is as shown below:

```cmd
chef-automate backup create
```

<!-- ### Restore

This section includes the procedure to restore backed-up data of the Chef Automate High Availability (HA) using File System.

Restore operation restores all the data while the backup is going on. The restore operation stops will the ongoing backup procedure. Let's understand the whole process by a scenario:

-   Create a automate _UserA_ and generate an API token named _Token1_ for _UserA_.
-   Create a backup, and let's assume the back id to be _20220708044530_.
-   Create a new user _UserB_ and a respective API token named _Token2_.
-   Now, suppose you want to restore data in the same automate cluster. In that case, the data will only be stored for _UserA_ with its token as the backup bundle only contains the _UserA_, and the _UserB_ is not available in the backup bundle. -->

### Restoring the Backed-up Data From file system

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

- Check the status of Automate HA Cluster from the bastion nodes by executing the `chef-automate status` command.

- Shutdown Chef Automate service on all front-end nodes

  - Execute `sudo systemctl stop chef-automate` command in all Chef Automate nodes
  - Execute `sudo systemctl stop chef-automate` command in all Chef Infra Server

- ssh to the one  of Chef Automate front-end node.

- Execute the restore command `chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --patch-config /etc/chef-automate/config.toml`.

{{< note >}}

After restore command successfully executed, we need to start the service's on other frontend node. use the below command to start all the service's
  
  ```sh
  sudo systemctl start chef-automate
  ```

{{< /note >}}