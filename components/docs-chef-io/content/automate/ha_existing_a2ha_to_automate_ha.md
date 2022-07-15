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

This page explains the procedure to migrate the existing A2HA data to the newly deployed Chef Automate HA. This migration involves the following steps:

### Prerequisites
- Mount the file system to Automate HA, which was mount to A2HA Cluster.
- Make sure that after mount, it should have the correct file permission. 


### Migration Steps 

1. Run the below command from any one of automate instance, in A2HA Cluster.
  ```cmd
    sudo chef-automate backup create
    sudo chef-automate bootstrap bundle create bootstrap.abb
  ```
  - First command will take the backup at mount file system. you can get the mount path from the file `/hab/a2_deploy_workspace/a2ha.rb`
  - Second command will create the bootstrap bundle, which we need to copy all the frontend nodes of Automate HA cluster. 
  - Once the backup is completed successfully, please save the backup Id. Example : `20210622065515`
2. Detach the File system from old A2HA cluster. 

3. Attach and Mount the same file system to Automate HA frontend and backend nodes at the location mention in the `config.toml` at field `backup_mount` 

4. Stop all the service's at frontend nodes in Automate HA Cluster.
  - Run the below command to all the Automate and Chef Infra Server nodes
  ``` bash
    sudo chef-automate stop
  ``` 

5. Please Get the Automate HA version number from the location `/var/tmp/` in Automate instance. Example : `frontend-4.x.y.aib`

6. Run the command at Chef-Automate node of Automate HA cluster to get the applied config
   ```bash
     sudo chef-automate config show > current_config.toml 
   ``` 
  Note: In Automate 4.x.y version onwards, opensearch credentials are not stored in the config. Add the Opensearch password to the generated config above.
  For example : 
  ```
    [global.v1.external.opensearch.auth.basic_auth]
      username = "admin"
      password = "admin"
  ```


7. To restore the A2HA backup on Chef Automate HA, run the following command from any Chef Automate instance of Chef Automate HA cluster:

  ```cmd
    sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
  ```

8.  - After the restore successfully executed, you wil see the below message
  
  ```
    Success: Restored backup 20210622065515
  ```

9. Copy the `bootstrap.abb` bundle to all the Frontend nodes of Chef Automate HA cluster. Unpack the bundle using below command on all the Frondend nodes.
 
  ```cmd
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
  ```

10. Start the Service in all the frontend nodes with below command.
  ``` bash
    sudo chef-automate start
  ``` 

{{< note >}}
  - After the restore command successfully executed. If we run the `chef-automate config show`
  we can see the both Elasticsearch and Opensearch config are the part of Automate Config. We can keep both the config, it wont' impact the functionality. After restore Automate HA talk to opensearch.

    OR

  -  We can remove the elaticsearch config from the automate, to do that redirect the applied config to the file and set the config again.
  ``` 
    chef-automate config show > applied_config.toml
  ```
  Modify the applied_config.toml, remove elastic search config and set the config.
  Need to set the applied_config.toml on all the frontend nodes manually. As removal of config is not supported from bastion. 
  Use the below command to set the config manually.
  ```
   chef-automate config set applied_config.toml
  ```
  
{{< /note >}}