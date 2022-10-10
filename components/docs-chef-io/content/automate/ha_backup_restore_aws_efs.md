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

- If user choose `backup_config` as `efs` in `config.toml,` backup is already configured during deployment, **the below steps are not required and can be skipped**. i.e., **`backup_config = "efs"`** . If we have kept the `backup_config` blank, then configuration needs to be configure manually.

{{< /note >}}

## Overview

A shared file system is always required to create **OpenSearch** snapshots. To register the snapshot repository using OpenSearch, it is necessary to mount the same shared filesystem to the exact location on all master and data nodes. Register the location in the `path.repo` setting on all master and data nodes.


### Setting up backup configuration

- Create a EFS file system, plese refer sample steps [here](https://docs.aws.amazon.com/efs/latest/ug/gs-step-two-create-efs-resources.html)
#### Configuration in Opensearch Node

- Mount the shared file system on all OpenSearch and Frontend servers:

```sh
mount /mnt/automate_backups
```

- Create an OpenSearch sub-directory and set permissions to one of the OpenSearch server (only if the network mount is correctly mounted).

```sh
sudo mkdir /mnt/automate_backups/opensearch
sudo chown hab:hab /mnt/automate_backups/opensearch/
```

Configure the OpenSearch `path.repo` setting by SSH to a single OpenSearch server by following the steps given below:

- Export the current OpenSearch config from the Habitat supervisor :

```sh
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-ha-opensearch | tail -n +2 > es_config.toml
```

- Edit `es_config.toml` and add the following settings to the end of the file.

{{< note >}} If the certificates have never been rotated, the above file may be empty. {{< /note >}}

```sh
[path]
# Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in config.toml
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

#### Healthcheck commands

```sh
    hab svc status (check whether OpenSearch service is up or not)
```

#### Configuration in Provision host

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

- Patch the `config` using below command.

```sh
./chef-automate config patch automate.toml
```

## Backup and Restore commands

### Backup

To create the backup, by running the backup command from a Chef Automate front-end node. The backup command is as shown below:

```cmd
chef-automate backup create
```

### Restoring the EFS Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

- Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

- Shutdown Chef Automate service on all front-end nodes

  - Execute `sudo systemctl stop chef-automate` command in all Chef Automate nodes
  - Execute `sudo systemctl stop chef-automate` command in all Chef Infra Server

- Log in to the same instance of Chef Automate front-end node from which backup is taken.

- Execute the restore command `chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --patch-config /etc/chef-automate/config.toml`.

{{< note >}}

After restore command successfully executed, we need to start the service's on other frontend node. use the below command to start all the service's
  
  ```sh
  sudo systemctl start chef-automate
  ```

{{< /note >}}
