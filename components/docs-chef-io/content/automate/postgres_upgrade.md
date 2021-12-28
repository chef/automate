+++
title = "Automate External Postgres Upgrade from v9.6 to v13"

date = 2018-03-26T15:27:52-07:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "External Postgres Upgrade"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/postgres_upgrade.md Automate External Postgres Upgrade"
    weight = 10
+++

## Upgrade path for Chef Automate users on Postgres v9.6
1. Take backup of chef automate - [Backup]({{< ref "backup.md" >}})

   * This is necessary just to ensure in case of any failure, there is a state to return to.

   * On Chef Automate machine

   * For Airgapped machine please follow [Airgapped Installation]({{< ref "airgapped_installation.md" >}})  to upgrade to latest version of Chef Automate which supports External Postgres v13.

   * For Non-Airgapped machine, Upgrade Chef Automate to latest version which supports External Postgresql v13:

    ```shell
    chef-automate upgrade run
    ```

   * After successful upgrade

   * Stop Chef Automate Services

    ```shell
    sudo chef-automate stop
    ```

3. Upgrade Postgesql 9.6 to 13.4

   * On External Postgres, please contact your database administrator to do the upgrade

   * On AWS RDS, please follow instructions on [Upgrading the PostgreSQL DB engine for Amazon RDS - Amazon Relational Database Service](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html)

1. On Chef Automate machine:

   * Start back Chef Automate

    ```shell
    sudo chef-automate start
    ```

   * Check all the services are up and running in Automate

    ```shell
    sudo chef-automate status
    ```


## Upgrade Failure Troubleshooting

1. If the upgrade failed and you are left with a corrupted Chef Automate and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF AUTOMATE**. Take all steps necessary to preserve the backup, including copying it to another disk. Consult with a professional sysadmin for instructions and best practices.

2. Contact customer support.

3. If you have configured the backup directory other than the default directory (`/var/opt/chef-automate/backups`), you must supply the backup directory path to the `backup restore` command as shown in the snippet below. Without a backup ID, Chef Automate uses the most recent backup in the backup directory.

To restore on a new host, run:

  ```shell
  chef-automate backup restore </path/to/backups/>BACKUP_ID
  ```
For other restoration types please refer this [Restore]({{< ref "restore.md" >}})

4. Do not continue upgrading PostgreSQL until you have an uncorrupted Chef Automate and an uncorrupted PostgreSQL database.