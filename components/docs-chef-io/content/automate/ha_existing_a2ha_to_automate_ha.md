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

* Backup the data from the existing A2HA using the following command:

  ```cmd
  sudo chef-automate backup create
  ```

  In case of multiple frontends, execute the above command from any frontend node(Chef Automate). The above command saves the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml`.

* Create a `.tar` file of the backup created above.

  For example: `tar -cvf backup.tar.gz path/to/backup/20201030172917/ /path/to/backup/automatebackup-elasticsearch/ /path/to/backup/.tmp/`

* Create a `.abb` file from any of the chef automate frontend nodes of A2HA. This creates a bundle of necessary keys, such as **pivotal.pem**, **secret key**, etc. Create a bundle for the file, as this might not be included in the normal backup process.

  ```cmd
  sudo chef-automate bootstrap bundle create bootstrap.abb
  ```

* Copy the `.tar` files created above to any Chef Automate HA(Chef Automate instances). Extract the `*.tar` file to the specific location mentioned in the `config.toml` file. For example: `/mnt/automate-backup`.

* To restore the A2HA backup on Chef Automate HA, run the following command from any chef-automate instance of Chef Automate HA:

  ```cmd
  sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config /etc/chef-automate/config.toml --airgap-bundle /var/tmp/frontend-4.x.y.aib --skip-preflight
  ```
* Copy the `.abb` bundle to all the Frontend nodes of Chef Automate HA. Unpack the bundle using below command on all the Frondend nodes.
 
  ```cmd
  sudo chef-automate bootstrap bundle unpack bootstrap.aib
  ```
