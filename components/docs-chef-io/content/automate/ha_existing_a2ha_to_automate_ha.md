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

  In case of multiple frontends, execute the above command from any frontend (Chef Automate). The above command saves the backup to the `/var/opt/chef-automate/backup` location unless you specify the location in `config.toml`.

* Create a `.tar` file of the backup created above. It is necessary to have a backup for the `.tar` directory, or the `metadata.json` might throw an error.

  For example: `tar -cvf backup.tar.gz path/to/backup/20201030172917/ /path/to/backup/automatebackup-elasticsearch/ /path/to/backup/.tmp/`

* Create a `.aib` file from any of the chef automate frontend nodes of A2HA. This creates a bundle of necessary keys, such as **pivotal.pem**, **secret key**, etc. Create a bundle for the file, as this might not be included in the normal backup process.

```cmd
sudo chef-automate bootstrap bundle create bootstrap.abb
```

* Copy the `.tar` and `.aib` files created above to any Automate HA Chef Automate instances. Extract the specific file to the specific location mentioned in the `config.toml` file. For example: `/mnt/automate-backup`. It is not mandatory to extract the backup to the specified location, but the restore command should always read the backup file from the location above.

* To restore the A2HA backup on Automate HA, run the following command:

  ```cmd
  sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config /etc/chef-automate/config.toml --airgap-bundle /var/tmp/frontend-20210624095659.aib --skip-preflight
  ```

  The above command generates the `frontend-20210624095659.aib` file in the new Automate HA file. All the services' habitat release versions can be considered during the restoration process. At the time of restoration, the A2HA restoration will try to find the A2HA habitat **pkg** version so there can be a possibility of different package versions. To get the error-free restoration, pass the current Automate HA packages (frontend `.aib` file), which is located in the `/var/tmp` directory of your Automate HA chef-automate instance.

* Unpack the `.aib` bootstrap file. The file will let you access the data in Chef Automate and will also let you view the login page.

* Copy the `bootstrap.aib` file to another automate node and chef node. You can copy this file even if you have multiple automate and chef instances. The secrets restored by unpacking the bootstrap file differ from other automated instances. So, sync all the automate and chef instances.

## Important commands and notes

To view what bootstrap includes in the `.aib` and `abb` files, run the following command:

```cmd
tail -n +3 bootstrap.aib | tar -tf -
```

`.aib` file only includes keys related data, whereas the `.abb` file also contains service packages.

To see the service data like secret service, search for it in the `/hab/svc` directory. This can be required if you want to compare `.aib` data with multiple FE (In respective chef-automate and chef-server) nodes.

**For Example:** For secret service cat `/hab/svc/secrets-service/data/secrets\_key`
