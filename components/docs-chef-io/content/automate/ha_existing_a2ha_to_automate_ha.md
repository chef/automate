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
  - First command will take the backup at mount file system. you can find the backup_config value from the file `/hab/a2_deploy_workspace/a2ha.rb`
  - Second command will create the bootstrap bundle, which we need to copy all the frontend nodes of Automate HA cluster. 

2. Mount the same file system to Automate HA frontend and backend nodes.

3. Stop all the service's at frontend nodes in Automate HA Cluster.
  - Run the below command to all the Automate and Chef Infra Server nodes
  ``` bash
    sudo chef-automate stop
  ``` 

4. To restore the A2HA backup on Chef Automate HA, run the following command from any chef-automate instance of Chef Automate HA:

  ```cmd
    sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config /etc/chef-automate/config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
  ```

5. Copy the `bootstrap.abb` bundle to all the Frontend nodes of Chef Automate HA. Unpack the bundle using below command on all the Frondend nodes.
 
  ```cmd
    sudo chef-automate bootstrap bundle unpack bootstrap.abb
  ```

6. Start the Service in All the Frontend Nodes with below command.
  ``` bash
    sudo chef-automate start
  ``` 
