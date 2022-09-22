+++
title = "Prerequisites"

draft = false

gh_repo = "automate"

[menu]
[menu.automate]
title = "Prerequisites"
identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_restore_prerequisites.md Backup and Restore Prerequisites"
parent = "automate/deploy_high_availability/backup_and_restore"
weight = 210
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

{{< note >}}

-   This page explains the prerequisites of the backup with AWS deployment procedure.
-   If user choose `backup_config` as `efs` in `config.toml,` backup is already configured during deployment, **the below steps are not required and can be skipped**. i.e., **`backup_config = "efs"`** . If we have kept the `backup_config` blank, we need to perform the below steps.

{{< /note >}}

### Overview

A shared file system is always required to create **OpenSearch** snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location (or one of its parent directories) in the `path.repo` setting on all master and data nodes.

Once the shared filesystem is mounted to `/mnt/automate_backups`, configure Automate to register the snapshot locations with OpenSearch.

-   Mount the shared file system on all OpenSearch servers:

```sh
mount /mnt/automate_backups
```

-   Create an OpenSearch sub-directory and set permissions to one of the OpenSearch server (only if the network mount is correctly mounted).

```sh
sudo mkdir /mnt/automate_backups/opensearch
sudo chown hab:hab /mnt/automate_backups/opensearch/
```

Configure the OpenSearch `path.repo` setting by SSH to a single OpenSearch server by following the steps given below:

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

To trigger the restart of the OpenSearch on each server, apply the updated `es_config.toml` config to OpenSearch once.

```sh
hab config apply automate-ha-opensearch.default $(date '+%s') es_config.toml

hab svc status (check whether OpenSearch service is up or not)

curl -k -X GET "<https://localhost:9200/_cat/indices/*?v=true&s=index&pretty>" -u admin:admin (Another way to check is to check whether all the indices are green or not)

# Watch for a message about OpenSearch going from RED to GREEN
`journalctl -u hab-sup -f | grep 'automate-ha-opensearch'
```

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

-   Patch the `.config` to trigger the deployment.

```sh
./chef-automate config patch automate.toml
```

## Backup and Restore

### Backup

Chef Automate let's you create a new backup. You can create it by running the backup command from a Chef Automate front-end node. The backup command is as shown below:

```cmd
chef-automate backup create
```

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
