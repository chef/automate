+++
title = "In-Place A2HA to Automate HA"

draft = false

gh_repo = "automate"
[menu]
[menu.automate]
title = "In-Place A2HA to Automate HA"
parent = "automate/deploy_high_availability/migration"
identifier = "automate/deploy_high_availability/migration/ha_inplace_migration.md In-Place A2HA to Automate HA"
weight = 200
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

{{< warning >}}

- A2HA user can be migrated to Automate HA with a minimum Chef Automate version [20201230192246](https://docs.chef.io/release_notes_automate/#20201230192246).

{{< /warning >}}


This page explains the In-Place migration of A2HA to Automate HA. This migration involves the following steps:

## Prerequisites

- A healthy state of the A2HA cluster to take fresh backup.
- A2HA is configured to take backup on a mounted network drive (location example: /mnt/automate_backup).
- Availability of 60% of space.

## Capture information about the current A2HA instance

In order to verify the migration is completed successfully we'll need to capture some information about the current installation. The following script will capture counts of objects in the Chef-Infra Server that we can compare with the server after the migration has been completed.

Create `capture_infra_counts.sh` and run it using `./capture_infra_counts.sh > pre_migration_infra_counts.log`

    ```bash
    #!/usr/bin/bash

    for i in `chef-server-ctl org-list`; do
        org=https://localhost/organizations/$i
        echo "Orgination: ${i}"
        echo -n "node count: "
        knife node list -s $org | wc -l
        echo -n "client count: "
        knife client list -s $org | wc -l
        echo -n "cookbook count: "
        knife cookbook list -s $org | wc -l
        echo -n "total objects: "
        knife list / -R -s $org | wc -l
        echo "----------------"
    done
    ```



## Taking Backup and clean up of instances

1. Take the latest backup of A2HA by running the following commands from any automate instance:

    ```cmd
    sudo chef-automate backup create
    ```

    The above command will store the backup in a configured backup patch in a2ha.rb config file `/hab/a2_deploy_workspace/a2ha.rb`. Once the backup is completed successfully, save the backup Id. For example: `20210622065515`. To use the backup created previously, run the following command on Automate node to get the backup id:

    ```sudo
    chef-automate backup list
    ```

    The output looks like as shown below:

    ```cmd
    Backup             State       Age
    20180508201548    completed  8 minutes old
    20180508201643    completed  8 minutes old
    20180508201952    completed  4 minutes old
    ```

1. Create a bootstrap bundle from one of automate node using the following command:

      ```cmd
      sudo chef-automate bootstrap bundle create bootstrap.abb
      ```

    The above command will create the bootstrap bundle and copy the same bundle to bastion or backup-dir.

1. Stop each of the frontend nodes (automate and chef-server) using the following command:

    ```cmd
    sudo chef-automate stop
    ```

    Rename `/hab` dir to something else like `/hab-old`.
 
    Remove the following files 
    * `/bin/chef-automate`
    * `/bin/hab`
    * `/bin/hab-launch`
    * `/bin/hab-sup`

1. Unload services from each of the Postgresql Nodes:

    ```cmd
    sudo hab svc unload chef/automate-backend-postgresql
    sudo hab svc unload chef/automate-backend-metricbeat
    sudo hab svc unload chef/automate-backend-journalbeat
    sudo hab svc unload chef/automate-backend-haproxy
    sudo hab svc unload chef/automate-backend-pgleaderchk
    ```

    Check the status using the `hab svc status` command. None of the services should be running. Once checked, stop the habitat supervisor with the command `systemctl stop hab-sup`. Rename /hab dir to something else like /hab-old.

1. Unload services from each of the Elasticsearch Nodes

    ```cmd
    sudo hab svc unload chef/automate-backend-elasticsidecar
    sudo hab svc unload chef/automate-backend-elasticsearch
    sudo hab svc unload chef/automate-backend-journalbeat
    sudo hab svc unload chef/automate-backend-metricbeat
    sudo hab svc unload chef/automate-backend-curator
    sudo hab svc unload chef/automate-backend-kibana
    ```

   Check the status using the `hab svc status` command. None of the services should be running. Once checked, stop the habitat supervisor with the command `systemctl stop hab-sup`. Rename /hab dir to something else like /hab-old.

1. In the **bastion** host, take a copy of your current workspace and keep it safe for a while.
1. Remove or Rename /hab dir in the bastion host.

## Installing the Latest Automate HA

Follow Automate HA installation documentation. Click [here](/automate/ha_onprim_deployment_procedure/) to know more about `config.toml`,
**provide** the same IPs and backup config in config.toml as in the  `a2ha.rb` file.

## File System backup configuration
In case the backup configuration was skipped in the deployment config.toml, the User needs to configure EFS backup manually in Automate HA please click [here](/automate/ha_backup_restore_file_system/#configuration-for-automate-node-from-provision-host) to know more.

{{<note>}}
While configuring the backup configuration provide the path of **Elasticsearch** instead of **Opensearch** as A2HA backup was in Elasticsearch directory 
like instead of `/mnt/automate_backups/opensearch/` it will be `/mnt/automate_backups/elasticsearch/`
{{</note>}}

## Restore Backup

Once deployment is successful, proceed with restoring the backup in Automate HA. Click [here](/automate/ha_backup_restore_file_system/) to know more.

Login to one of automate nodes, and take **current_config.toml** file as shown below:

```sudo
sudo chef-automate config show > current_config.toml
```

Find the following config in the **current_config.toml** file and update it to look like the following:

```cmd
[global.v1.external.opensearch.auth.basic_auth]
    username = "admin"
    password = "admin"
```

AND

```cmd
[global.v1.external.opensearch.backup.fs]
    path = "/mnt/automate_backups/elasticsearch"
```

Copy the **bootstrap.abb** bundle to all the Frontend nodes of the Chef Automate HA cluster. Unpack the bundle using the below command on all the Frontend nodes:

```cmd
sudo chef-automate bootstrap bundle unpack bootstrap.abb
```

To restore, use the below command from same automate node, Make sure to **stop all other frontend nodes using `chef-automate stop`**:

```cmd
sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
```

{{< note >}}

- After the restore command is successfully executed, run the `chef-automate config show` command. Both the ElasticSearch and OpenSearch configs are part of Automate Config. Keep both configs; it won't impact the functionality. After restoring Automate HA is configured to communicate with OpenSearch.

OR

- We can remove the ElasticSearch config from the automate. To do that, redirect the applied config to the file and set the config again.

```bash
chef-automate config show > applied_config.toml
```

Modify `applied_config.toml`, remove the elastic search config, and set the config. Set `applied_config.toml` on all the frontend nodes manually. As the removal of config is not supported from the bastion. Use the below command to set the config manually.

```bash
chef-automate config set applied_config.toml
```

{{< /note >}}


Click [here](/automate/ha_backup_restore_object_storage/) to know more about the usage of S3 backup.

{{< note >}}
1. Once Automate HA is up and running with restored data, We can remove old backed-up directories sudo `rm -rf hab-old`, freeing up acquired space.
1. Reset the backup configuration path to Opensearch so that new backups will be stored in Opensearch directory, please click [here](/automate/ha_backup_restore_file_system/#configuration-for-automate-node-from-provision-host) to know more.
{{< /note >}}

## Validate successful migration

1. Check the Automate UI of Automate HA. Check whether the data is present in Automate UI for HA.
1. If you are using the embedded chef server, log in to the Chef Server HA node, and run the following script to get a count of objects from the Chef Infra Server, this should match the counts captured at the start of the migration

    Create `capture_infra_counts.sh` and run it using `./capture_infra_counts.sh > post_migration_infra_counts.log`

    ```bash
    #!/usr/bin/bash

    for i in `chef-server-ctl org-list`; do
        org=https://localhost/organizations/$i
        echo "Orgination: ${i}"
        echo -n "node count: "
        knife node list -s $org | wc -l
        echo -n "client count: "
        knife client list -s $org | wc -l
        echo -n "cookbook count: "
        knife cookbook list -s $org | wc -l
        echo -n "total objects: "
        knife list / -R -s $org | wc -l
        echo "----------------"
    done
    ```

    Compare the pre migration to post migration counts `diff pre_migration_infra_counts.log post_migration_infra_counts.log`

1. Connect Chef-Workstation to the new cluster and use knife to communicate with Automate HA

    1. Open the `~/.chef/config.rb`, `~/.chef/knife.rb` or `~/.chef/credentials` file from a Chef-Workstation and update the `chef_server_url` with the Automate fqdn.

        Example: `chef_server_url          "https://<automate-fqdn>/organizations/new_org"`

    1. Run `knife user list`, `knife node list`, or `knife cookbook list` and verify the commands complete successfully

## Troubleshoot

1. While installing the new Automate HA, if PostgreSQL is having any issues in starting, and in PostgreSQL instance `hab svc status` shows a secret key mismatch error, then try the cleanup command with new Automate HA cli `chef-automate cleanup --onprem-deployment` and then remove `/bin/chef-automate` from all frontend nodes, now try the installation again.

1. Click [here](/automate/ha_existing_a2ha_to_automate_ha/#troubleshooting) to know more if you encounter an error while restoring related to the ElasticSearch snapshot.
2. While restoring the backup if an error related to backup directory occurs like 
> **Error in Automate node:** failed to create snapshot repository: Elasticsearch repository create request failed for repo**
> OR
> **Error in Opensearch node:** /mnt/automate_backups/backups/automate-elasticsearch-data/chef-automate-*-service] doesn't match any of the locations specified by path.repo

please re-check your EFS backup configuration for the Automate and OpenSearch node, click [here](/automate/ha_backup_restore_file_system/#configuration-for-automate-node-from-provision-host) to know more.
