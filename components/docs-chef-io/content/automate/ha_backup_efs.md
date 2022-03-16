+++
title = "Taking Backup with EFS System"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Taking Backup with EFS System"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_backup_efs.md Taking Backup with EFS System"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 220
+++

This page explains how to take backup for External Elastic Search (ES) and Postgres-Sql on External File-System (EFS). You can take the backup on EFS system through DNS or IP.

A shared file system requires you to create Elasticsearch snapshots. To mount the same shared filesystem to the same location on all master and data nodes, register these snapshot repositories with Elasticsearch.

You must register this location (or one of its parent directories) in the `path.repo` setting on all master and data nodes.

{{< note >}}

Ensure you perform the backup configuration before deploying the Chef Automate High Availability (HA) cluster.

{{< /note >}}

## Pre-Backup Configurations

- Create the EFS over the AWS.
- Open the port `2049` Proto(NFS) for the EFS security group.

## Backup Procedure

Let us assume that the shared filesystem is mounted to `/mnt/automate_backups`. Now, follow these steps to configure the Chef Automate High Availability (HA) to register the snapshot locations with Elasticsearch:

1. Execute the `mount /mnt/automate_backups` command to ensure the shared file system exists on all Elasticsearch servers.

1. Create Elasticsearch sub-directory and set permissions by executing the following commands:

```bash
sudo mkdir /mnt/automate_backups/elasticsearch
sudo chown hab:hab /mnt/automate_backups/elasticsearch/
```

{{< note >}}

If the network appropriately mounted, you need to perform this step on a single Elasticsearch server.

{{< /note >}}

1. Export the current Elasticsearch configuration from the Habitat supervisor.

1. Log in as a root user.

1. SSH to a single Elasticsearch server and configure Elasticsearch `path.repo` setting by executing the following commands:

```bash
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-backend-elasticsearch | tail -n +2 > es_config.toml
```

1. Edit `es_config.toml` to add the following settings at the end of the file:

```ruby
[es_yaml.path]
   # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in /hab/a2_deploy_workspace/a2ha.rb
   repo = "/mnt/automate_backups/elasticsearch"
```

{{< note >}}

This file may be empty if credentials are never rotated.

{{< /note >}}

1. Apply updated `es_config.toml` configuration to Elasticsearch by executing the following commands:

```bash
hab config apply automate-backend-elasticsearch.default $(date '+%s') es_config.toml
hab svc status (check elasticsearch service is up or not)
curl -k -X GET "https://localhost:9200/_cat/indices/*?v=true&s=index&pretty" -u admin:admin
   # Watch for a message about Elasticsearch going from RED to GREEN
`journalctl -u hab-sup -f | grep 'automate-ha-elasticsearch'
```

You can perform this application only once, which triggers a restart of the Elasticsearch services on each server.

1. Configure Chef Automate HA to handle external Elasticsearch backups by adding the following configuration to `/hab/a2_deploy_workspace/config/automate.toml` on the provisioning host or from the bastion host:

```ruby
[global.v1.external.elasticsearch.backup]
   enable = true
   location = "fs"

   [global.v1.external.elasticsearch.backup.fs]
   # The `path.repo` setting you've configured on your Elasticsearch nodes must be
   # a parent directory of the setting you configure here:
   path = "/mnt/automate_backups/elasticsearch"

   [global.v1.backups.filesystem]
   path = "/mnt/automate_backups/backups"
```

1. Execute the `./chef-automate config patch automate.toml` command to apply the patch configuration to the Chef Automate HA servers. This command also triggers the deployment.

1. Execute the `chef-automate backup create` command from a Chef Automate front-end node to create a backup.
