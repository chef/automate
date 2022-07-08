+++
title = "Backup and Restore"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Backup and Restore"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_restore.md Backup and Restore"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 230
+++

## Backup

Chef Automate let's you create a new backup. You can create it by running the backup command from a Chef Automate front-end node. The backup command is as shown below:

```cmd
chef-automate backup create
```

## Restore

This section includes the procedure to restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS) and the Amazon Web Services (AWS) S3 bucket.

## Note

Restore operation will restore all data until backup had been taken, after any data or operation will be lost
- example
  - We created automate user A and user B is created and we have generated a API token as tok1 
  - We took backup with id 20220708044530
  - Again we created users C and D, and alo generated a API token tok2,
  - Now we are restored data in same automate cluster, then only user A and B will be restored similarly API token tok1 will be restored, 
  - But user C and D along with API token tok2 will be lost because we had created these after backup it was not part of restore.


### Restoring the S3 Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using the Amazon Web Services (AWS) S3 bucket, follow the steps given below:

- Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

- Shutdown Chef Automate service on all front-end nodes
  - Execute `sudo systemctl stop chef-automate` command in all Chef Automate nodes
  - Execute `sudo systemctl stop chef-automate` command in all Chef Infra Server 

- Log in to the same instance of Chef Automate front-end node from which backup is taken.

- Execute the restore command `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"`.

- Start all Chef Automate and Chef Infra Server front-end nodes by executing the `sudo systemctl start chef-automate` command.

### Restoring the EFS Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

- Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

- Shutdown Chef Automate service on all front-end nodes 
  - Execute `sudo systemctl stop chef-automate` command in all Chef Automate nodes
  - Execute `sudo systemctl stop chef-automate` command in all Chef Infra Server 

- Log in to the same instance of Chef Automate front-end node from which backup is taken.

- Execute the restore command `chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --patch-config /etc/chef-automate/config.toml`.

{{< figure src="/images/automate/ha_restore.png" alt="Restore">}}

- Start all Chef Automate and Chef Infra Server front-end nodes by executing the `sudo systemctl start chef-automate` command.

{{< figure src="/images/automate/ha_restore_success.png" alt="Restore Success">}}
