+++
title = "Restoring the S3 Backed-up Data"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Restoring the S3 Backed-up Data"
    identifier = "automate/deploy_high_availability/backup_and_restore/ha_restore_efs.md Restoring the S3 Backed-up Data"
    parent = "automate/deploy_high_availability/backup_and_restore"
    weight = 230
+++

This page includes the procedure to restore backed-up data of the Chef Automate High Availability (HA) using EFS [External File System].

1. Check the status of all Chef Automate and Chef Infra Server front-end nodes by executing the command `chef-automate status`.

1. Shutdown Chef Automate service on all front-end nodes by executing the command `sudo systemctl stop chef-automate`.

1. Log in to the same instance of Chef Automate front-end node from which backup is taken.

1. Execute the restore command `chef-automate backup restore s3://bucket_name/path/to/backups/BACKUP_ID --skip-preflight --s3-access-key "Access_Key"  --s3-secret-key "Secret_Key"`.

1. Start all Chef Automate and Chef Infra Server front-end nodes by executing the command `sudo systemctl start chef-automate`.
