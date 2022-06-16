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
 
## Upgrading with FileSystem Backup

Here we expect both the versions of Chef Automate and Chef Automate HA are same. Chef Automate HA is only available in version 4.x.

1. Create Backup of Chef Automate using command:
  ```bash
  chef-automate backup create
  chef-automate bootstrap bundle create bootstrap.abb
  ```
  This command saves the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml`.
2. Create Bundle using this command:
  ```bash
  tar -cvf backup.tar.gz path/to/backup/<backup_id>/ /path/to/backup/automatebackup-elasticsearch/ /path/to/backup/.tmp/
  ```
3. Transfer this tar bundle to one of the Chef Automate Nodes.
4. Transfer `.abb` file to all the FrontEnd Nodes (both Chef Automate and Chef Infra Server).
4. Login to that Chef Automate Node with ssh.
5. Unzip the bundle using:
  ```bash
  tar -xf backup.tar.gz -C /mnt/automate_backups
  ```
5. Restore using this command:
  ```bash
  automate_version_number=4.0.91 ## please chnage this based on the version of Chef Automate running.
  chef-automate backup restore /mnt/automate_backups/backups/<backup_id>/ --patch-config /etc/chef-automate/config.toml --airgap-bundle /var/tmp/frontend-${automate_version_number}.aib --skip-preflight
  ```
6. Upack the `.abb` file on all the Frontend nodes: \
  Login to Each Frontend Node and then run after copying the `.abb` file.
  ```bash
  chef-automate bootstrap bundle unpack bootstrap.abb
  ```