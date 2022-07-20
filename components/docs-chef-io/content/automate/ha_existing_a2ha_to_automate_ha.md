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

This page explains migrating the existing A2HA data to the newly deployed Chef Automate HA. This migration involves the following steps:

## Prerequisites

- Mount the file system to Automate HA, which was mounted to A2HA Cluster.
- Make sure that it has the correct file permission after mount.

## Migration

1. Run the following commands from any automate instance in A2HA Cluster.

```cmd
sudo chef-automate backup create
sudo chef-automate bootstrap bundle create bootstrap.abb
```

- The first command will take the backup at the mount file system. You can get the mount path from the file `/hab/a2_deploy_workspace/a2ha.rb`
- The second command will create the bootstrap bundle, which we need to copy all the frontend nodes of Automate HA cluster.
- Once the backup is completed successfully, please save the backup Id. For example: `20210622065515`.

2. Detach the File system from the old A2HA cluster.

3. Attach and Mount the same file system to Automate HA frontend and backend nodes at the location mentioned in the `config.toml` at field `backup_mount`.

4. Stop all the services at frontend nodes in Automate HA Cluster.

- Run the below command to all the Automate and Chef Infra Server nodes
  
``` bash
sudo chef-automate stop
```

5. Please Get the Automate HA version number from the location `/var/tmp/` in Automate instance. Example : `frontend-4.x.y.aib`.

6. Run the command at the Chef-Automate node of Automate HA cluster to get the applied config

```bash
sudo chef-automate config show > current_config.toml 
``` 

{{< note >}}

In Automate **4.x.y** version onwards, OpenSearch credentials are not stored in the config. Add the OpenSearch password to the generated config above. For example :

```bash
[global.v1.external.opensearch.auth.basic_auth]
username = "admin"
password = "admin"
```

{{< /note >}}

7. To restore the A2HA backup on Chef Automate HA, run the following command from any Chef Automate instance of the Chef Automate HA cluster:

```cmd
sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
```

8. After the restore is successfully executed, you will see the below message:
  
```bash
Success: Restored backup 20210622065515
```

9. Copy the `bootstrap.abb` bundle to all the Frontend nodes of the Chef Automate HA cluster. Unpack the bundle using the below command on all the Frontend nodes.

```cmd
sudo chef-automate bootstrap bundle unpack bootstrap.abb
```

10. Start the Service in all the frontend nodes with the below command.

``` bash
sudo chef-automate start
```

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
    "location" : "/mnt/automate_backups/automate-elasticsearch-data/chef-automate-es6-compliance-service",
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

- After the restore command is successfully executed. If we run the `chef-automate config show`, we can see that both ElasticSearch and OpenSearch config are part of Automate Config. We can keep both the config; it won't impact the functionality. After restoring Automate HA talk to OpenSearch.

OR

-  We can remove the elaticsearch config from the automate. To do that, redirect the applied config to the file and set the config again.

```bash
chef-automate config show > applied_config.toml
```

Modify `applied_config.toml`, remove elastic search config, and set the config. Set `applied_config.toml` on all the frontend nodes manually. As the removal of config is not supported from the bastion. Use the below command to set the config manually.

```bash
chef-automate config set applied_config.toml
```
  
{{< /note >}}
