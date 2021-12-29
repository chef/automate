+++
title = "Upgrade Services in Automate"

date = 2018-03-26T15:27:52-07:00
draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Upgrade Services"
    parent = "automate/configuring_automate"
    identifier = "automate/configuring_automate/upgrade_services.md"
+++

This guide covers upgrading services used by Chef Automate.

## Upgradethe PostgreSQL for Amazon RDS

1. For AWS RDS, please follow instructions on [Upgrading the PostgreSQL DB engine for Amazon RDS - Amazon Relational Database Service](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html)

## Upgrade External PostgreSQL from 9.6 to 13.3

Chef Automate and Chef Infra Server both use PostgreSQL as the primary database for storing node data. [PostgreSQL 9.6 is EOL](https://endoflife.date/postgresql) and Chef customers running Chef Automate with PostgreSQL 9.6 should upgrade to [Postgres 13](https://www.postgresql.org/about/news/postgresql-13-released-2077/) immediately.

Amazon Web Services Relational Database Service (AWS RDS) should upgrade  by Monday, January 17. Amazon is automatically migrating all AWS RDS customers from PostgreSQL from 9.6 to 13 starting Tuesday, January 18, 2022.

### Migration Planning

The upgrade process for PostgreSQL from 9.6 to 13.3 requires a one-time downtime to vacuum, upgrade, and re-index the database. The entire upgrade operation takes about five minutes for each 2GB of data in PostgreSQL. This process may take longer depending on your server hardware and the size of the node objects in Chef Automate.

TODO: Does this setting exist in Chef Automate?

{{< note >}}

Set the `postgresql['pg_upgrade_timeout']` attribute in [chef-server.rb]({{< relref "config_rb_server_optional_settings" >}}) to the timeout value for the upgrade. Set this value based on the size of your data, where it take about one minute per 1,000 nodes which is approximately 286MB.

{{</note >}}

{{< note >}}
While doing our testing we had major steps like:
1) Vacuum all data in PostgreSQL 9.6
2) pg_upgrade check
3) pg_upgrade
4) reindexing
{{< /note >}}

### Backup Chef Automate

{{% danger %}}
**BACKUP CHEF AUTOMATE AND SECURE THE DATA**. Preserve your backup at all costs. Copy the backup to a second and sepatate location.
{{% /danger %}}

1. Backup [Backup]({{< ref "backup.md" >}}) Chef Automate
   Database migrations have inherent risk to your system. Create a backup before beginning any migration or update. This ensures that you have a recoverable state in case any step in the process fails. Copy the backup to a another disk that is not connected to Chef Automate.

### Upgrade Chef Automate

1. Upgrade Chef Automate to the latest version
    1. For most Chef Automate installations, run the upgrade command:

        ```shell
        chef-automate upgrade run
        ```

        After a successful upgrade, stop Chef Automate Services.

        ```shell
        sudo chef-automate stop
        ```

    1. For **airgapped** Chef Automate systems, follow the instructions in the [Airgap Installation Guide]({{< ref "airgapped_installation.md" >}}) to upgrade to the latest version

### Prepare PostgreSQL 9.6 for Migration

For External PostgreSQL, please contact your database administrator (DBA) to perform the upgrade. The following External PostgreSQL upgrade steps are provided as a courtesy. It is the responsibility of the user to upgrade and maintain any External PostgreSQL configurations.

1. Run `VACUUM FULL` on the PostgreSQL database if you don't have automatic vacuuming set up. This process will reduce the size of the database by deleting unnecessary data and speeds up the migration. The `VACUUM FULL` operation takes around 1 to 2 minutes per gigabyte of data depending on the complexity of the data, and requires free disk space at least as large as the size of your database.

  ```bash
     sudo su - opscode-pgsql
     /opt/opscode/embedded/bin/vacuumdb --all --full
  ```

  You should then see output similar to:

  ```bash
     vacuumdb: vacuuming database "bifrost"
     vacuumdb: vacuuming database "oc_id"
     vacuumdb: vacuuming database "opscode-pgsql"
     vacuumdb: vacuuming database "opscode_chef"
     vacuumdb: vacuuming database "postgres"
     vacuumdb: vacuuming database "template1"
  ```

### Migrate to PostgreSQL 13.3

1. Log into the external PostgreSQL machine.
1. Update packages and install your selected PostgreSQL version.

  Example (Ubuntu/PostgreSQL 13.3):

  ```bash
  sudo apt-get update
  sudo apt-get install postgresql-13
  ```

1. Check if there are any differences in the config files. Make sure to update the new config files if required.

   Example (PostgreSQL 13.3):

   ```bash
   diff /etc/postgresql/OLD_POSTGRESQL_VERSION/main/postgresql.conf /etc/postgresql/13/main/postgresql.conf
   diff /etc/postgresql/OLD_POSTGRESQL_VERSION/main/pg_hba.conf     /etc/postgresql/13/main/pg_hba.conf
   ```

1. Stop the PostgreSQL service.

   ```bash
   sudo systemctl stop postgresql.service
   ```

1. Log in as the `postgres` user.

   ```bash
   su postgres
   ```

1. Ensure that you are in a directory where you can run the `pg_upgrade` command.

   Example:

   ```bash
   cd /tmp
   ```

1. Check clusters (notice the `--check` argument, this will not change any data).
   Example (PostgreSQL 13.3):

   ```bash
   /usr/lib/postgresql/13/bin/pg_upgrade \
   --old-datadir=/var/lib/postgresql/9.6/main \
   --new-datadir=/var/lib/postgresql/13/main \
   --old-bindir=/usr/lib/postgresql/9.6/bin \
   --new-bindir=/usr/lib/postgresql/13/bin \
   --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
   --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf' \
   --check
   ```

### Migrate to PostgreSQL 13.3

1. Migrate the data (without the `--check` argument).
   Example (PostgreSQL 13.3):

   ```bash
   /usr/lib/postgresql/13/bin/pg_upgrade \
   --old-datadir=/var/lib/postgresql/9.6/main \
   --new-datadir=/var/lib/postgresql/13/main \
   --old-bindir=/usr/lib/postgresql/9.6/bin \
   --new-bindir=/usr/lib/postgresql/13/bin \
   --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
   --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf'
   ```

1. Log out of the `postgres` user.

   ```bash
   exit
   ```

1. Swap the ports for the old and new PostgreSQL versions.

   Example (PostgreSQL 13.3):

   ```bash
   sudo vim /etc/postgresql/13/main/postgresql.conf
   # change "port = 5433" to "port = 5432"

   sudo vim /etc/postgresql/9.6/main/postgresql.conf
   # change "port = 5432" to "port = 5433"
   ```

1. Start the PostgreSQL service.

   ```bash
   sudo systemctl start postgresql.service
   ```

1. Log in as the `postgres` user and confirm that the new PostgreSQL version is correct.
   Example (PostgreSQL 13.3):

   ```bash
   sudo su - postgres
   psql -c "SELECT version();"

   ```

   Outputs somthing like:

   ```
                                                                      version
   ---------------------------------------------------------------------------------------------------------------------------------------------
    PostgreSQL 13.3 (Ubuntu 13.3-1.pgdg16.04+1) on x86_64-pc-linux-gnu, compiled by gcc (Ubuntu 5.4.0-6ubuntu1~16.04.12) 5.4.0 20160609, 64-bit
   (1 row)

   ```

### Reindex PostgreSQL 13.3

1. Run `reindexdb`. Example:

   ```bash
   $ /usr/bin/reindexdb --all
   reindexdb: reindexing database "bifrost"
   reindexdb: reindexing database "oc_id"
   reindexdb: reindexing database "opscode_chef"
   reindexdb: reindexing database "postgres"
   reindexdb: reindexing database "template1"
   ```

1. Log into the Chef Infra Server machine.

1. Check the status of Chef Infra Server. PostgreSQL should be connected.

TODO: What are the commands for this in Automate?

   ```bash
   $ sudo chef-server-ctl status
   -------------------
    Internal Services
   -------------------
   run: bookshelf: (pid 15763) 219163s; run: log: (pid 16559) 228464s
   run: elasticsearch: (pid 15797) 219162s; run: log: (pid 16345) 228507s
   run: nginx: (pid 15901) 219162s; run: log: (pid 16745) 228452s
   run: oc_bifrost: (pid 15909) 219161s; run: log: (pid 16237) 228519s
   run: oc_id: (pid 15915) 219161s; run: log: (pid 16255) 228513s
   run: opscode-erchef: (pid 15948) 219160s; run: log: (pid 16673) 228458s
   run: redis_lb: (pid 15952) 219160s; run: log: (pid 16779) 228445s
   -------------------
    External Services
   -------------------
   run: postgresql: connected OK to 10.0.11.0:5432
   ```

4. Restart Chef Automate

  Run the start command:

    ```bash
    sudo chef-automate start
    ```

  Check all services are running correctly:

    ```shell
    sudo chef-automate status
    ```

## Upgrade Failure Troubleshooting

1. If the upgrade failed and you are left with a corrupted Chef Automate and/or a corrupted database, **DO NOT RISK YOUR BACKUP OF AUTOMATE**. Take all steps necessary to preserve the backup, including copying it to another disk. Take all steps necessary to preserve the backup, including copying it to another disk.
2. Contact Chef customer support.
3. If you have configured the backup directory other than the default directory (`/var/opt/chef-automate/backups`), you must supply the backup directory path to the `backup restore` command as shown in the snippet below. Without a backup ID, Chef Automate uses the most recent backup in the backup directory.\
To restore on a new host, run:

      ```shell
      chef-automate backup restore </path/to/backups/>BACKUP_ID
      ```

      For other restoration types please refer the [Restore]({{< ref "restore.md" >}}) documentation.
4. Do not continue upgrading PostgreSQL until you have an uncorrupted Chef Automate and an uncorrupted PostgreSQL database.
