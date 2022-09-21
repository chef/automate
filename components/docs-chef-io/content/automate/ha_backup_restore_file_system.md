+++
title = "Backup and restore using Filesystem | On-Permise Deployment"

draft = false

gh_repo = "automate"

[menu]
[menu.automate]
title = "Prerequisites"
identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore File System - On-Premise"
parent = "automate/deploy_high_availability/backup_and_restore"
weight = 210
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

{{< note >}}

-   This page explains the configuration for backup with file system and Restore with On-Premise deployment procedure.
-   If user choose `backup_config` as `file_system` in `config.toml,` backup is already configured during the deployment, and in that case **the below steps are not required and can be skipped**. i.e., **`backup_config = "file_system"`** . If `backup_config` left blank, then configuration needs to be done for backup and restore.

{{< /note >}}

## Overview

A shared file system is always required to create **OpenSearch** snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location (or one of its parent directories) in the `path.repo` setting on all master and data nodes.

### Setting up backup configuration

#### Configuration in Opensearch Node

-   Mount the shared file system on **all** OpenSearch servers:

    ```sh
    mount /mnt/automate_backups
    ```

{{< note >}}

-   For the sake of this walk through let us assume `/mnt/automate_backups` as a backup path (which is also a default path for AWS deployment)

{{< /note >}}

Apply following steps on any **one of the OpenSearch server** node

-   Create an OpenSearch sub-directory and set permissions (only if the network mount is correctly mounted).

    ```sh
    sudo mkdir /mnt/automate_backups/opensearch
    sudo chown hab:hab /mnt/automate_backups/opensearch/
    ```

Configure the OpenSearch `path.repo` setting by following the steps given below:

-   Export the current OpenSearch config from the Habitat supervisor. Get the root access to run the following commands:

    ```sh
    source /hab/sup/default/SystemdEnvironmentFile.sh
    automate-backend-ctl applied --svc=automate-ha-opensearch | tail -n +2 > es_config.toml
    ```

-   Edit `es_config.toml` and add the following settings to the end of the file.

    {{< note >}} If the credentials have never been rotated, the above file may be empty. {{< /note >}}

    ```sh
      [path]
      # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in /hab/a2_deploy_workspace/a2ha.rb
      repo = "/mnt/automate_backups/opensearch"
    ```

-   The following command will apply the updated `es_config.toml` config to OpenSearch once and will trigger restart of opensearch in all nodes.

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

    -   Configure Automate to handle _External OpenSearch Backups_.

    -   Create an `automate.toml` file on the provisioning server using the following command:

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

    -   Patch the `automate.toml` config to trigger the deployment from provision host.

    ```sh
    ./chef-automate config patch automate.toml
    ```

## Backup and Restore

### Backup

Chef Automate let's you create a new backup. You can create it by running the backup command from a Chef Automate front-end node. The backup command is as shown below:

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

#### Restoring the EFS Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

-   Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

-   Shutdown Chef Automate service on all front-end nodes

    -   Execute `sudo systemctl stop chef-automate` command in all Chef Automate nodes
    -   Execute `sudo systemctl stop chef-automate` command in all Chef Infra Server

-   Log in to the same instance of Chef Automate front-end node from which backup is taken.

-   Execute the restore command `chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --patch-config /etc/chef-automate/config.toml`.

{{< figure src="/images/automate/ha_restore.png" alt="Restore">}}

-   Start all Chef Automate and Chef Infra Server front-end nodes by executing the `sudo systemctl start chef-automate` command.

{{< figure src="/images/automate/ha_restore_success.png" alt="Restore Success">}}
