+++
title = "A2HA to Automate HA Inplace Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "A2HA to Automate HA Inplace Upgrade"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/inplace_upgrade_a2ha_to_automate_ha.md A2HA to Automate HA Inplace Upgrade"
    weight = 200
+++

{< warning >}}
Inplace migration have more downtime, so we suggest to go for normal migration process.
{{< /warning >}}

This page explains upgrading inplace A2HA to Automate HA. This migration involves the following steps:

## Prerequisites

- Mount a efs/nfs file system A2HA Cluster to take backup.
- Make sure that mount have the correct file permission after mount.

## Taking backup from old A2HA

1. Run the following commands from any elsaticsearch instance in A2HA cluster
```cmd
sudo mkdir /mnt/automate_backups/elasticsearch
sudo chown hab:hab /mnt/automate_backups/elasticsearch/
source /hab/sup/default/SystemdEnvironmentFile.sh
automate-backend-ctl applied --svc=automate-backend-elasticsearch | tail -n +2 > es_config.toml
```

2. Add following line in es_config.toml file
```cmd
   [es_yaml.path]   
   # Replace /mnt/automate_backups with the backup_mount config found on the provisioning host in /hab/a2_deploy_workspace/a2ha.rb   
   repo = "/mnt/automate_backups/elasticsearch" 
```

3. Now apply the config from elasticsearch node 
```cmd
   hab config apply automate-backend-elasticsearch.default $(date '+%s') es_config.toml
   journalctl -u hab-sup -f | grep 'automate-backend-elasticsearch'
   # Watch for a message about Elasticsearch going from RED to GREEN
```

4. From bastion machine add the following configuration to /hab/a2_deploy_workspace/config/automate.toml

```cmd
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
5. Now from bastion node workspace directory (/hab/a2_deploy_workspace) run the following command
```cmd 
    automate-cluster-ctl deploy
```

5. Run the following commands from any automate node of A2HA Cluster.

```cmd
sudo chef-automate backup create
sudo chef-automate bootstrap bundle create bootstrap.abb
```

6. Copy above bootstrap.abb file to /mnt/automate_backups

7. Verify the backup in /mnt/automate_backups/bakups/20210622065515

## In-Place upgrade

1. From all A2HA automate nodes
```cmd
    sudo systemctl stop chef-automate
    sudo rm -rf /hab
    sudo rm -rf /var/automate-ha
    sudo rm -rf /var/tmp/*
    sudo lsof -n -i :9631 | grep LISTEN
    sudo kill -i <pid>
```
2. From all A2HA chef_server nodes
```cmd
    sudo systemctl stop chef-automate
    sudo rm -rf /hab
    sudo rm -rf /var/automate-ha
    sudo rm -rf /var/tmp/*
    sudo lsof -n -i :9631 | grep LISTEN
    sudo kill -i <pid>
```
3. From all A2HA postgresql nodes
```cmd
    sudo systemctl stop hab-sup
    sudo rm -rf /hab
    sudo rm -rf /var/automate-ha
    sudo rm -rf /var/tmp/*
    sudo lsof -n -i :9631 | grep LISTEN
    sudo kill -i <pid>
```

4. From all A2HA elasticsearch nodes
```cmd
    sudo systemctl stop hab-sup
    sudo rm -rf /hab
    sudo rm -rf /var/automate-ha
    sudo rm -rf /var/tmp/*
    sudo lsof -n -i :9631 | grep LISTEN
    sudo kill -i <pid>
```

5. From A2HA bashtion host node run the following command from home directory or any directory other then /hab/*
```cmd
    cp /hab/a2-deploy-worksapce/a2ha.rb .
    wget https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip
    unzip chef-automate_linux_amd64.zip
    cp -f chef-automate /usr/bin/chef-automate
    wget https://packages.chef.io/airgap_bundle/current/automate/latest.aib
    rm -rf /hab
```

6. Generate config from bashtion host
```cmd
    chef-automate init-config-ha existing_infra
```

7. Edit config and provide following details same as a2ha.rb of A2HA cluster. 
    provide No. of nodes
    IP address
    FQDN
    ssh username
    ssh key paths 

```cmd
    vi config.toml
```

8. Confirm all the data in the config is correct:
```cmd
cat config.toml
```

9. Run Deploy command
```cmd
chef-automate deploy config.toml --airgap-bundle latest.aib
```

10. After Deployment is done successfully. Check status of Chef Automate HA services:
```cmd
chef-automate status
```


## Restoring Old A2HA data to inplace upgraded Automate HA

- To restore old A2HA data to Inplace upgraded Automate HA run the following command

1. From Chef-Automate instance take current config in file
```cmd
sudo chef-automate config show > current_config.toml 
```

{< note >}}

In Automate **4.x.y** version onwards, OpenSearch credentials are not stored in the config. Add the OpenSearch password to the generated current_config.toml above. For example :

```bash
[global.v1.external.opensearch.auth.basic_auth]
username = "admin"
password = "admin"
```

{{< /note >}}


2. Stop chef-automate from all Automate nodes and All Chef_Server node
```cmd
    sudo chef-automate stop
```

3. To restore the A2HA backup on Chef Automate HA, run the following command from any Chef Automate instance of the Chef Automate HA cluster

```cmd
sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
```

4. After the restore is successfully executed, you will see the below message:
  
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
