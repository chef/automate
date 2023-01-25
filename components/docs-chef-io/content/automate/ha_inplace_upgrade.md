+++
title = "In-Place Upgrade A2HA to Automate HA"

draft = false

gh_repo = "automate"
[menu]
[menu.automate]
title = "In-Place Upgrade A2HA to Automate HA"
parent = "automate/deploy_high_availability/migration"
identifier = "automate/deploy_high_availability/migration/ha_inplace_upgrade.md In-Place Upgrade A2HA to Automate HA"
weight = 200
+++

This page explains In-Place migration of A2HA to Automate HA This migration involves the following steps:

## Prerequisites

- A2HA cluster should be in healthy state

- A2HA is configured to take backup on mounted network drive (location example : /mnt/automate_backup).

## Taking Backup and clean up of instances.

1. Take latest backup of A2HA by running the following commands from any automate instance.

    ```cmd
    sudo chef-automate backup create
    ```
    Above command will store backup in configured backup patch in a2ha.rb config file `/hab/a2_deploy_workspace/a2ha.rb`
    once the backup is completed successfully, save the backup Id. For example: `20210622065515`.
   If you want to use backup created previously run the command on Automate node, to get the backup id
   ```chef-automate backup list```

          
          $ chef-autoamte backup list
          Backup             State       Age
          20180508201548    completed  8 minutes old
          20180508201643    completed  8 minutes old
          20180508201952    completed  4 minutes old
          
2. Create bootstrap bundle

      ```cmd 
      sudo chef-automate bootstrap bundle create bootstrap.abb 
      ```

    Above command will create the bootstrap bundle, copy this bastion or backup dir 

3.  Stop each of frontend nodes (automte and chef-server) 

     ```cmd 
      sudo chef-automate stop
      ```
      rename /hab dir to something else like /hab-old
      remove /bin/chef-automate

4.  Unload services from each of Postgresql Nodes
    ```cmd
    sudo hab svc unload chef/automate-backend-postgresql
    sudo hab svc unload chef/automate-backend-metricbeat
    sudo hab svc unload chef/automate-backend-journalbeat
    sudo hab svc unload chef/automate-backend-haproxy
    sudo hab svc unload chef/automate-backend-pgleaderchk
    ```
    check status use ``` hab svc status ``` none of services should be running
    then stop habitat superviser with command ``` systemctl stop hab-sup ```
    rename /hab dir to something else like /hab-old


5. Unload services from each of Elasticsearch Nodes
   ```cmd
    sudo hab svc unload chef/automate-backend-elasticsidecar
    sudo hab svc unload chef/automate-backend-elasticsearch
    sudo hab svc unload chef/automate-backend-journalbeat
    sudo hab svc unload chef/automate-backend-metricbeat
    sudo hab svc unload chef/automate-backend-curator
    sudo hab svc unload chef/automate-backend-kibana
   ```
   check status use ``` hab svc status ``` none of services should be running
   then stop habitat superviser with command ``` systemctl stop hab-sup ```
   rename /hab dir to something else like /hab-old


6. In bastion host take copy of your current workspace and keep it safe for while
7. remove or rename /hab dir in bashtion host

## Installing Latest Automate HA
Follow Automate HA installation documentations Click [here](/automate/ha_onprim_deployment_procedure/) to know more

in config.toml give same automate IPs as it was in a2ha.rb file

## Restore Backup 

once deployment is successful, 
Now proceed with restore the backup in Automate HA, 
login to one of automate node and take current_config,toml file
``` sudo chef-automate config show > current_config.toml ```

add the following config to current_config.toml

```cmd
[global.v1.external.opensearch.auth.basic_auth] 
    username = "admin"
    password = "admin"
```

and

```cmd
[global.v1.external.opensearch.backup.fs]
    path = "/mnt/automate_backups/elasticsearch"
```

to restore use below command
```cmd
sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
```

Click [here](/automate/ha_backup_restore_file_system/) to know more

{{< note >}}

- After the restore command is successfully executed. If we run the `chef-automate config show`, we can see that both ElasticSearch and OpenSearch config are part of Automate Config. We can keep both the config; it won't impact the functionality. After restoring Automate HA, talk to OpenSearch.

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

{{< note >}}
** In case of ``` backup_config = "file_system" ``` filed had be give in config.toml of Automate HA deployment, then need to patch below opensearch config from bastion before starting restore.

-   Create a .toml (say os_config.toml) file in **the Provision host** and copy the following template with the path to the repo.
    ```sh
      [path]
      repo = "/mnt/automate_backups/elasticsearch"
    ```
-   Following command will add the configuration to the OpenSearch node.
    ```sh
      chef-automate config patch --opensearch <PATH TO OS_CONFIG.TOML>
    ```

{{< /note >}}



In case of S3 backup use Click [here](/automate/ha_backup_restore_object_storage/) to know more


Copy the bootstrap.abb bundle to all the Frontend nodes of the Chef Automate HA cluster. Unpack the bundle using the below command on all the Frontend nodes

```cmd
sudo chef-automate bootstrap bundle unpack bootstrap.abb
```


## Troubleshoot

1. While installing new Automate HA if postgresql not getting started, and in postgesql instance ``` hab svc status ``` shows secret key mismatch error then try cleanup command with new Autoamte HA cli ``` chef-automate cleanup --onprem-deployment``` and then remove /bin/chef-automate from all frontend nodes, now try installation again

2. While doing restore if any error related to elasticsearch snapshot please refer Click [here](/automate/ha_existing_a2ha_to_automate_ha/#troubleshooting) to know more
