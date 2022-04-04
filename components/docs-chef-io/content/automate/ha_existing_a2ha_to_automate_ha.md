+++
title = "Existing A2HA to Automate HA"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Existing A2HA to Automate HA"
    parent = "automate/deploy_high_availability/migration"
    identifier = "automate/deploy_high_availability/migration/ha_existing_a2ha_to_automate_ha.md Existing A2HA to Automate HA"
    weight = 210
+++

This page explains the procedure to migrate the existing A2HA data to the newly deployed Chef Automate HA. This migration involves following steps:

* Backup the data from the existing A2HA using the following command:

  ```cmd
  sudo chef-automate backup create
  ```

  In case of multiple frontends, the above command can be executed from any of the front end (Chef Automate).  The above command saves the backup to the `/var/opt/chef-automate/backup location` unless specific location is specified in `config.toml`.

* Create a `.tar` file of the backup created above. It is necessary to have a back for the `.tar` directory or the `metadata.json` might throw some error.

  For example: `tar -cvf backup.tar.gz path/to/backup/20201030172917/ /path/to/backup/automatebackup-elasticsearch/ /path/to/backup/.tmp/`

* Create a `.aib` file from any of chef-automate frontend node of A2HA. This creates a bundle of all necessary keys, such as **pivotal.pem**, **secret key**, etc. Create a bundle for the file as this might not be included in regular backup process.

```cmd
sudo chef-automate bootstrap bundle create bootstrap.abb
```

* Copy the `.tar` and `.aib` file created above to any of the Automate HA Chef Automate instance. Extract the specific file to the specific location mentioned in `config.toml` file. For example: `/mnt/automate-backup`. It is not mandatory to extract the backup to the specified location but the restore command should always read the backup file from the above defined location.

* 

Restore A2HA backup on Automate HA. Read this docs for chef-automate restore. In below command there is also a frontend-20210624095659.aib generated in new Automate HA file mention that's because while restoration we also keep in mind all the services habitat release version. Because during restoration time A2HA restoration will try to find A2HA habitat pkg version so there can be a scenario occure where all(A2HA and automate HA frontends (automate's)) packages version can't be the same. That's why we are passing current Automate HA packages. You can find frontend aib file in /var/tmp directory of your Automate HA chef-automate instance.

E.g. sudo chef-automate backup restore /mnt/automate_backups/backups/20210622065515/ --patch-config /etc/chef-automate/config.toml --airgap-bundle /var/tmp/frontend-20210624095659.aib --skip-preflight

After that if you not do this step then you will might face warning when you'll try to open chef-automate UI It looks like you do not have permission to access any data in automate So make sure you have unpacked the. aib file. Otherwise you'll not see login page. To unpack the bootstrap file that we copied from A2HA chef-automate using below command

Sudo chef-automate bootstrap bundle unpack bootstrap.abb

Copy the bootstrap.aib file to another automate node and chef node also if you are having multiple automate and chef instances. Because the secrets we have restored by unpacking the bootstrap file would be different for another automate instance. So we need to make that all the automate and chef instance would be in sync.

Important command and notes

Using the below command you can see what bootstrap includes in aib file and abb file. Aib file will only include keys related data while abb file will include service packages also. You can use below command and can compare both the file's data

tail -n +3 bootstrap.aib | tar -tf -

After using above command, if you want to see the data of service like secret service then you would see those services into /hab/svc directory. This'll be needed if you want to compare aib data between multiple FE(In respective chef-automate and chef-server) nodes.

E.g For secret service cat /hab/svc/secrets-service/data/secrets\_key

