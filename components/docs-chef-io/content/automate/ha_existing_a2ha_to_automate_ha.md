+++
title = "Existing A2HA to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Existing A2HA to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_existing_a2ha_to_automate_ha.md Existing A2HA to Automate HA"
    weight = 200
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

{{< warning >}}

- A2HA user can be migrated to Automate HA with a minimum Chef Automate version [20201230192246](https://docs.chef.io/release_notes_automate/#20201230192246).

{{< /warning >}}

This page explains migrating the existing A2HA data to the newly deployed Chef Automate HA. This migration involves the following steps:

## Prerequisites

- Ability to mount the file system, which was mounted to A2HA Cluster for backup purposes, to Automate HA.
- Configure the A2HA to take backup on a mounted network drive (location example: `/mnt/automate_backup`).

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

## Migration

1. Run the following commands from any automate instance in A2HA Cluster.

    ```cmd
    sudo chef-automate backup create
    sudo chef-automate bootstrap bundle create bootstrap.abb
    ```

    - The first command will take the backup at the mount file system. You can get the mount path from the file `/hab/a2_deploy_workspace/a2ha.rb`
    - The second command will create the bootstrap bundle, which is needed to copy all the frontend nodes of Automate HA cluster.
    - Once the backup is completed successfully, save the backup Id. For example: `20210622065515`.

    - If you want to use the backup created previously, run the command on Automate node to get the backup id
      `chef-automate backup list`

    ```sh
    Backup             State       Age
    20180508201643    completed  8 minutes old
    20210622065515    completed  4 minutes old
    ```

1. Detach the File system from the old A2HA cluster.

1. Configure the backup at Automate HA cluster. If you have not configured it, please refer to this [Doc: Pre Backup Configuration for File System Backup](/automate/ha_backup_restore_file_system/#setting-up-the-backup-configuration)

1. From Step 3, you will get the backup mount path.

1. Stop all the services at frontend nodes in Automate HA Cluster.

1. Get the Automate version from the location `/var/tmp/` in Automate instance. Example: `frontend-4.x.y.aib`.

1. Run the command at the Chef-Automate node of Automate HA cluster to get the applied config:

    ```bash
    sudo chef-automate config show > current_config.toml 
    ```

    - Run the below command to all the Automate and Chef Infra Server nodes

    ``` bash
    sudo chef-automate stop
    ```

1. To run the restore command, we need the airgap bundle. Get the Automate HA airgap bundle from the location `/var/tmp/` in Automate instance. Example: `frontend-4.x.y.aib`.
    - In case of airgap bundle is not present at `/var/tmp`, in that case, we can copy the bundle from the bastion node to the Automate node.

1. Run the command at the Chef-Automate node of Automate HA cluster to get the applied config

    ```bash
    sudo chef-automate config show > current_config.toml 
    ```

1. Add the OpenSearch credentials to the applied config.

    - If using Chef Managed Opensearch, add the config below into `current_config.toml` (without any changes).  

        ```bash
        [global.v1.external.opensearch.auth.basic_auth]
            username = "admin"
            password = "admin"
        ```

    - In case `backup_config = "file_system"` had been provided in config.toml of Automate HA deployment, then please patch the below OpenSearch config from bastion before starting the restore.
        - Create a toml (say os_config.toml) file from **provision host** and copy the following template with the path to the repo. Update the repo path, what you have created at the time of deployment `</mnt/automate_backups/elasticsearch>`.

        ```sh
        [path]
        repo = "/mnt/automate_backups/elasticsearch"
        ```

      - Following command will add the configuration to the OpenSearch node.

        ```sh
        chef-automate config patch --opensearch <PATH TO OS_CONFIG.TOML>
        ```

    - If using AWS Managed services, then add the below config into `current_config.toml` (change this with your actual credentials)

{{< warning >}}
{{% automate/char-warn %}}
{{< /warning >}}

```bash
[global.v1.external.opensearch.auth]
    scheme = "aws_os"
[global.v1.external.opensearch.auth.aws_os]
    username = "THIS YOU GET IT FROM AWS Console"
    password = "THIS YOU GET IT FROM AWS Console"
    access_key = "<YOUR AWS ACCESS KEY>"
    secret_key = "<YOUR AWS SECRET KEY>"
```

1. Copy the `bootstrap.abb` bundle to all the Frontend nodes of the Chef Automate HA cluster. Unpack the bundle using the below command on all the Frontend nodes.

    ```cmd
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
    ```
2. Stop the Service in all the frontend nodes with the below command.

    ``` bash
    sudo chef-automate stop
    ```

3. To restore the A2HA backup on Chef Automate HA, run the following command from any Chef Automate instance of the Chef Automate HA cluster:

    ```cmd
    sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
    ```

4. After successfully executing the restore, you will see the below message:

    ```bash
    Success: Restored backup 20210622065515
    ```

5. Start the Service in all the frontend nodes with the below command.

    ``` bash
    sudo chef-automate start
    ```

    {{< warning >}}

    - After the restore command is successfully executed. If we run the `chef-automate config show`, we can see that both ElasticSearch and OpenSearch config are part of Automate Config. After restoring Automate HA talk to OpenSearch.

    - Remove the elastic search config from all Frontend nodes; to do that, redirect the applied config to the file and set the config again. For example:

    ```bash
    chef-automate config show > applied_config.toml
    ```

    Remove the below field from the `applied_config.toml`.

    ```bash
    [global.v1.external]
     [global.v1.external.elasticsearch]
       enable = true
       nodes = [""]
       [global.v1.external.elasticsearch.auth]
       scheme = ""
       [global.v1.external.elasticsearch.auth.basic_auth]
           username = ""
           password = ""
       [global.v1.external.elasticsearch.ssl]
       root_cert = ""
       server_name = ""
    ```

    Apply this modified config by running below command.

    ```bash
    chef-automate config set applied_config.toml
    ```

    Executed the steps from the Front end nodes.

    {{< /warning >}}

## Equivalent Commands

In Automate HA there are equivalent command which had been used in A2HA:

| Commands                   | A2HA                                                      | Automate HA                                                   |
|----------------------------|-----------------------------------------------------------|---------------------------------------------------------------|
| init config existing infra | `bash automate-cluster-ctl config init -a existing_nodes` | `bash chef-automate init-config-ha existing_infra `           |
| deploy                     | `bash automate-cluster-ctl deploy `                       | `bash chef-automate deploy config.toml`                       |
| info                       | `bash automate-cluster-ctl info `                         | `bash chef-automate info`                                     |
| status                     | `bash chef-automate status `                              | `bash chef-automate status`                                   |
| ssh                        | `bash automate-cluster-ctl ssh <name> `                   | `bash chef-automate ssh --hostname <name>`                    |
| test                       | `bash automate-cluster-ctl test `                         | `bash chef-automate test`                                     |
| gather logs                | `bash automate-cluster-clt gather-logs `                  | `bash chef-automate gather-logs`                              |
| workspace                  | `bash automate-cluster-clt workspace `                    | `bash chef-automate workspace [OPTIONS] SUBCOMMAND [ARG] ...` |

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

## Troubleshooting

**In case of Restore failure from ElasticSearch to OpenSearch**

> **Error: Failed to restore a snapshot**

Get the basepath location from the A2HA Cluster using the curl request below.

> **REQUEST**

```bash
curl -XGET http://localhost:10144/_snapshot/_all?pretty -k 
```

> **RESPONSE**

Look for the `location` value in the response.

```json
"settings" : {
    "location": "/mnt/automate_backups/automate-elasticsearch-data/chef-automate-es6-compliance-service",
}
```

`location` value should be matched with the OpenSearch cluster. In case of `location` value is different, use the below script to create the snapshot repo.

```bash
indices=(
chef-automate-es5-automate-cs-oc-erchef
chef-automate-es5-compliance-service
chef-automate-es5-event-feed-service
chef-automate-es5-ingest-service
chef-automate-es6-automate-cs-oc-erchef
chef-automate-es6-compliance-service
chef-automate-es6-event-feed-service
chef-automate-es6-ingest-service
)

for index in ${indices[@]}; do

curl -XPUT -k -H 'Content-Type: application/json' http://localhost:10144/_snapshot/$index --data-binary @- << EOF
{
  "type": "fs",
  "settings": {
    "location" : "/mnt/automate_backups/automate-elasticsearch-data/$index"
  }
}
EOF
done
```

{{< note >}}

- After the restore command is successfully executed. If we run the `chef-automate config show`, we can see that both ElasticSearch and OpenSearch config are part of Automate Config. We can keep both config; it won't impact the functionality. After restoring Automate HA, talk to OpenSearch.

OR

- We can remove the ElasticSearch config from the automate. To do that, redirect the applied config to the file and set the config again.

```bash
chef-automate config show > applied_config.toml
```

Modify `applied_config.toml`, remove elastic search config, and set the config. Set `applied_config.toml` on all the frontend nodes manually. As the removal of config is not supported from the bastion. Use the below command to set the config manually.

```bash
chef-automate config set applied_config.toml
```

{{< /note >}}
