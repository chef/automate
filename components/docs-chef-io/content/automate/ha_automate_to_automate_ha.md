+++
title = "Automate to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Automate to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_automate_to_automate_ha.md Automate to Automate HA"
    weight = 220
+++
 
## Upgrading with FileSystem Backup locally

Here we expect both the versions of Standalone Chef Automate and Chef Automate HA are same. Chef Automate HA is only available in version 4.x.

1. Create Backup of Chef Automate Standalone using command:
    ```bash
     chef-automate backup create
     chef-automate bootstrap bundle create bootstrap.abb
    ```
  - The First command will create the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml`
  - Second command will create the `bootstrap.abb` 
  - Once the backup is completed successfully, please save the backup Id. Example : `20210622065515`

2. Create Bundle using this command:
    ```bash
     tar -cvf backup.tar.gz path/to/backup/<backup_id>/ /path/to/backup/automatebackup-elasticsearch/ /path/to/backup/.tmp/
    ```
3. Transfer this `tar` bundle to one of the Chef Automate HA Frontend Nodes.

4. Transfer `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server).

5. Go the Chef Automate HA, where we copied the `tar` file. Unzip the bundle using:
    ```bash
     tar -xf backup.tar.gz -C /mnt/automate_backups
    ```

6. Stop all the service's at frontend nodes in Automate HA Cluster.
   Run the below command to all the Automate and Chef Infra Server nodes
    ``` bash
      sudo chef-automate stop
    ``` 

7. Run the command at Chef-Automate node of Automate HA cluster to get the applied config
   ```bash
     sudo chef-automate config show > current_config.toml 
   ``` 

8. Restore in Chef-Automate HA using this command:
    ```bash
     automate_version_number=4.0.91 ## please change this based on the version of Chef Automate running.
     
     chef-automate backup restore /mnt/automate_backups/backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
    ```
9. Upack the `bootstrap.abb` file on all the Frontend nodes: \
  Login to Each Frontend Node and then run after copying the `bootstrap.abb` file.
    ```bash
     chef-automate bootstrap bundle unpack bootstrap.abb
    ```
10. Start the Service in All the Frontend Nodes with below command.
    ``` bash
     sudo chef-automate start
    ``` 

## Upgrading with FileSystem Backup via volume mount

Here we expect both the versions of Standalone Chef Automate and Chef Automate HA are same. Chef Automate HA is only available in version 4.x.

1. Create Backup of Chef Automate Standalone using command:
    ```bash
     chef-automate backup create
     chef-automate bootstrap bundle create bootstrap.abb
    ```
  - First command will create the backup at the file mount location mention in the `config.toml`
  - Second command will create the `bootstrap.abb` 
  - Once the backup is completed successfully, please save the backup Id. Example : `20210622065515`

2. Detach the File system from Standlalone Chef-Automate. 

3. Attach and Mount the same file system to the Automate-HA all the nodes:
  - Make sure that it should have permission for hab user

3. Stop all the service's at frontend nodes in Automate HA Cluster.
   Run the below command to all the Automate and Chef Infra Server nodes
    ``` bash
      sudo chef-automate stop
    ``` 

4. Please Get the Automate HA version number from the location `/var/tmp/` in Automate instance. Example : `frontend-4.x.y.aib`

5. Run the command at Chef-Automate node of Automate HA cluster to get the applied config
   ```bash
     sudo chef-automate config show > current_config.toml 
   ``` 

6. Run the restore command in one of the Chef Automate node in Chef-Automate HA cluster :
    ```bash
     chef-automate backup restore /mnt/automate_backups/backups/<backup_id>/ --patch-config current_config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
    ```

7. Copy the `bootstrap.abb` file to all the Chef Automate HA FrontEnd Nodes (both Chef Automate and Chef Infra Server).

8. Upack the `bootstrap.abb` file on all the Frontend nodes.
   ssh to Each Frontend Node and then run below command
    ```bash
     chef-automate bootstrap bundle unpack bootstrap.abb
    ```
9. Start the Service in All the Frontend Nodes with below command.
    ``` bash
      sudo chef-automate start
    ``` 

