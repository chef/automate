+++
title = "External PostgreSQL Upgrade"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Upgrade External PostgreSQL"
    identifier = "automate/configuring_automate/postgresql/postgres_external_upgrade.md PostgreSQL Upgrade"
    parent = "automate/configuring_automate/postgresql"
    weight = 20
+++

This guide covers upgrading services used by Chef Automate.

## Upgrade Amazon RDS for PostgreSQL

Amazon Web Services Relational Database Service (AWS RDS) should upgrade by Monday, January 17. Amazon is automatically migrating all AWS RDS customers from PostgreSQL from 9.6 to 13 starting Tuesday, January 18, 2022.

To upgrade AWS RDS, please follow instructions on [Upgrading the PostgreSQL DB engine for Amazon RDS - Amazon Relational Database Service](https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_UpgradeDBInstance.PostgreSQL.html)

## Upgrade External PostgreSQL from 9.6 to 13.4

Chef Automate uses PostgreSQL as the primary database for storing node data. [PostgreSQL 9.6 is EOL](https://endoflife.date/postgresql) and Chef customers running Chef Automate with PostgreSQL 9.6 should upgrade to [Postgres 13](https://www.postgresql.org/about/news/postgresql-13-released-2077/).

### Migration Planning

The upgrade process for PostgreSQL from 9.6 to 13.4 requires a one-time downtime to vacuum, upgrade, and re-index the database. The entire upgrade operation takes about five minutes for each 2GB of data in PostgreSQL. This process may take longer depending on your server hardware and the size of the node objects in Chef Automate.

### Requirements

{{% warning %}}
Upgrading PostgreSQL upgrades the database for all connected services. If you have multiple services connected to PostgreSQL, make sure that you have stopped the other services and prepared them for the upgrade.
{{% /warning %}}

This upgrade guide is for systems running:

- A Single PostgreSQL 9.6 installation
- Using Ubuntu 18.04 or higher
- On a virtual machine such as an EC2 instance or on a single physical machine
- Enough free space to run a second copy of the data that is in the existing PostgreSQL 9.6 installation. This upgrade requires a minimum of 55% free space on the machine.

### Backup Chef Automate

{{% danger %}}
**BACKUP CHEF AUTOMATE AND SECURE THE DATA**. Preserve your backup at all costs. Copy the backup to a second and separate location.
{{% /danger %}}

Database migrations have inherent risk to your system. Create a backup before beginning any migration or update. This ensures that you have a recoverable state in case any step in the process fails. Copy the backup to a another disk that is not connected to Chef Automate. This ensures that you have state to restore, in case of a failure in the upgrade process.

Follow the [Backup]({{< relref "backup.md" >}}) documentation to make a copy of your Chef Automate data.

### Upgrade Chef Automate

- For **airgapped** Chef Automate systems, follow the instructions in the [Airgap Installation Guide]({{< ref "airgapped_installation.md" >}}) to upgrade to the latest version.

- Upgrade standard Chef Automate installations to the latest version that supports External PostgreSQL v13 with the command:

```bash
chef-automate upgrade run
```

### Stop Chef Automate

After successful upgrade, stop Chef Automate Services.

```bash
sudo chef-automate stop
```

### Upgrade from PostgreSQL 9.6 to 13.4

#### Install PostgreSQL 13.4

1. SSH or Login to the machine running PostgreSQL 9.6 with user who has sudo or root access.

    ```bash
    ssh -i "<xyz.pem>" <sudo_username>@<postgres_machine_ip>
    ```

1. Install PostgreSQL v13

    ```bash
    sudo apt-get update
    sudo apt-get install postgresql-13
    ```

1. If the PostgreSQL v9.6 config was modified or customized. Please check the differences and update the new PostgreSQL v13 config with similar changes. Please connect with your database administrator if you don't know these changes.

    ```bash
    sudo sdiff -s /etc/postgresql/9.6/main/postgresql.conf /etc/postgresql/13/main/postgresql.conf
    sudo sdiff -s /etc/postgresql/9.6/main/pg_hba.conf /etc/postgresql/13/main/pg_hba.conf
    ```

### Stop PostgreSQL Services

1. Stop Both PostgreSQL Servers:

    ```bash
    sudo systemctl stop postgresql.service
    ```

1. Login as the `postgres` user

    ```bash
    sudo su - postgres
    ```

### Prepare the Database for Migration

Run `vacuumdb --all --full` on the PostgreSQL database if you don't have automatic vacuuming set up. This process will reduce the size of the database by deleting unnecessary data and speeds up the migration. This operation takes around 1 to 2 minutes per gigabyte of data depending on the complexity of the data, and requires at least as much free disk space as the size of your database.

For more information on upgrading using `vacuumdb` see the PostgreSQL 13 documentation for [vacuumdb](https://www.postgresql.org/docs/13/app-vacuumdb.html).

1. Run Vacuum DB before moving data from PostgreSQL v9.6 to v13

    ```bash
    vacuumdb --all --full
    ```

    Expected output:

    ```bash
    vacuumdb: vacuuming database "automate-cs-bookshelf"
    vacuumdb: vacuuming database "automate-cs-oc-bifrost"
    vacuumdb: vacuuming database "automate-cs-oc-erchef"
    vacuumdb: vacuuming database "chef_applications_service"
    vacuumdb: vacuuming database "chef_authn_service"
    vacuumdb: vacuuming database "chef_authz_service"
    vacuumdb: vacuuming database "chef_cereal_service"
    vacuumdb: vacuuming database "chef_compliance_service"
    vacuumdb: vacuuming database "chef_config_mgmt_service"
    vacuumdb: vacuuming database "chef_infra_proxy"
    vacuumdb: vacuuming database "chef_ingest_service"
    vacuumdb: vacuuming database "chef_license_control_service"
    vacuumdb: vacuuming database "chef_session_service"
    vacuumdb: vacuuming database "chef_teams_service"
    vacuumdb: vacuuming database "chef_user_settings_service"
    vacuumdb: vacuuming database "data_feed_service"
    vacuumdb: vacuuming database "dex"
    vacuumdb: vacuuming database "nodemanager_service"
    vacuumdb: vacuuming database "notifications_service"
    vacuumdb: vacuuming database "postgres"
    vacuumdb: vacuuming database "secrets_service"
    vacuumdb: vacuuming database "template1"
    ```

#### Upgrade

For more information on upgrading using `pg_upgrade` and `pg_upgrade --check` see the PostgreSQL 13 documentation for [pg_upgrade](https://www.postgresql.org/docs/13/pgupgrade.html).

1. Run pg_upgrade check command.

    ```bash
    cd ~
    /usr/lib/postgresql/13/bin/pg_upgrade \
    --old-datadir=/var/lib/postgresql/9.6/main \
    --new-datadir=/var/lib/postgresql/13/main \
    --old-bindir=/usr/lib/postgresql/9.6/bin \
    --new-bindir=/usr/lib/postgresql/13/bin \
    --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
    --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf' \
    --check
    ```

1. Migrate the Data (run pg_upgrade command without --check):

    ```bash
    cd ~
    /usr/lib/postgresql/13/bin/pg_upgrade \
    --old-datadir=/var/lib/postgresql/9.6/main \
    --new-datadir=/var/lib/postgresql/13/main \
    --old-bindir=/usr/lib/postgresql/9.6/bin \
    --new-bindir=/usr/lib/postgresql/13/bin \
    --old-options '-c config_file=/etc/postgresql/9.6/main/postgresql.conf' \
    --new-options '-c config_file=/etc/postgresql/13/main/postgresql.conf'
    ```

#### Configure

1. Exit the PostgreSQL shell

    ```bash
    exit
    ```

1. Swap the ports of PostgreSQL v9.6 and 13.4:

    ```bash
    PG_9_6_PORT=5432 # Assuming Postgres v9.6 is currently running on this port
    PG_13_PORT=5433 # Assuming Postgres 13 is currently running on this port
    sudo sed -i "s/port = $PG_9_6_PORT/port = $PG_13_PORT/g" /etc/postgresql/9.6/main/postgresql.conf
    sudo sed -i "s/port = $PG_13_PORT/port = $PG_9_6_PORT/g" /etc/postgresql/13/main/postgresql.conf
    ```

1. Start both PostgreSQL Servers:

    ```bash
    sudo systemctl start postgresql.service
    ```

#### Validate

1. Confirm the installed version:

    ```bash
    sudo su - postgres
    psql -c "SELECT version();"
    ```

1. Run the generated `analyze_new_cluster.sh` script

    ```bash
    cd ~
    ./analyze_new_cluster.sh
    ```

    Reindexing is not required for Chef Automate. If `pg_upgrade` reported errors or need for reindexing please refer to [pg_upgrade documentation](https://www.postgresql.org/docs/13/pgupgrade.html) for details.

1. Exit postgres user:

    ```bash
    exit
    ```

#### Cleanup PostgreSQL 9.6

After you have confirmed the installation and you no longer need PostgreSQL 9.6, remove PostgreSQL v9.6 and its data:

```bash
sudo su - postgres
./delete_old_cluster.sh
```

### Restart Chef Automate

1. Start Chef Automate:

    ```bash
    sudo chef-automate start
    ```

1. Check that the Chef Automate services are up and running:

    ```bash
    sudo chef-automate status
    ```

## Troubleshoot Upgrade Failures

1. If the upgrade failed and you are left with a corrupted Chef Automate or a corrupted PostgreSQL database, **DO NOT RISK YOUR BACKUP OF AUTOMATE**. Take all steps necessary to preserve the backup, including copying it to another disk.
1. Contact Chef customer support.
1. If you have configured the backup directory as a location other than the default directory (`/var/opt/chef-automate/backups`), you must supply the backup directory path to the `backup restore` command as shown in the snippet below. Without a backup ID, Chef Automate uses the most recent backup in the backup directory.

   To restore on a new host, run:

   ```bash
   chef-automate backup restore </path/to/backups/>BACKUP_ID
   ```

      For other restoration types please refer this [Restore]({{< ref "restore.md" >}})

1. Do not upgrade PostgreSQL until you have an uncorrupted Chef Automate and an uncorrupted PostgreSQL database.
