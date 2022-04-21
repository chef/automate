+++
title = "Restore"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Restore"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_restore_s3.md Restore"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 230
+++

This page includes the procedure to restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS) and the Amazon Web Services (AWS) S3 bucket.

## Restoring the S3 Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using External File System (EFS), follow the steps given below:

- Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

- Shutdown Chef Automate service on all front-end nodes by executing the `sudo systemctl stop chef-automate` command.

- Log in to the same instance of Chef Automate front-end node from which backup is taken.

- Execute the restore command `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"`.

- Start all Chef Automate and Chef Infra Server front-end nodes by executing the `sudo systemctl start chef-automate` command.

## Restoring the EFS Backed-up Data

To restore backed-up data of the Chef Automate High Availability (HA) using the Amazon Web Services (AWS) S3 bucket, follow the steps given below:

- Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the `chef-automate status` command.

- Shutdown Chef Automate service on all front-end nodes by executing the `sudo systemctl stop chef-automate` command.

- Log in to the same instance of Chef Automate front-end node from which backup is taken.

- Execute the restore command `chef-automate backup restore <BACKUP-ID> --yes -b /mnt/automate_backups/backups --patch-config /etc/chef-automate/config.toml`.

{{< figure src="/images/automate/ha_restore.png" alt="Restore">}}

- Start all Chef Automate and Chef Infra Server front-end nodes by executing the `sudo systemctl start chef-automate` command.

{{< figure src="/images/automate/ha_restore_success.png" alt="Restore Success">}}
