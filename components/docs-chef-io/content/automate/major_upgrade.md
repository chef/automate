+++
title = "Upgrade to Chef Automate 3"
date = 2022-03-03T12:02:46-08:00
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Upgrade to 3.x"
    identifier = "automate/upgrade/major_upgrade.md Major Upgrade"
    parent = "automate/upgrade"
    weight = 20
+++

Chef Automate provides an entire suite of enterprise capabilities for node visibility and compliance. Chef Automate upgrades from one minor version to another automatically. However, Chef Automatate will not automatically upgrade to a major version. See the instructions below for manually upgrading Chef Automate from date-based versions to Chef Automate 3.

## Upgrade Journey

Please choose following upgrade journey based on your current version of Chef Automate. All of these upgrades are manual upgrades.

| Your Current Version | Upgrade To |
| -------------------- | ---------- |
| Any version before 20220329091442| 20220329091442|
| 20220329091442| 3.0.1|

For example, if today you are on version 2021201164433, then your upgrade journey should be:

1. Manual upgrade to 20220329091442.
1. Manual upgrade to 3.0.1.

## Prerequisites

- **Plan your downtime**: This upgrade requires downtime. Before upgrading, set the environment to handle the downtime.
- **Backup Chef Automate database**: This Chef Automate version upgrades PostgreSQL. [Backup](/automate/backup/) your data before upgrading.

## Upgrade Path

There are four possible upgrade scenarios:

- [Chef Automate with Embedded PostgreSQL]({{< relref "#chef-automate-with-embedded-postgresql" >}})
- [Chef Automate with External PostgreSQL]({{< relref "#chef-automate-with-external-postgresql" >}})
- [Chef Automate in Air-Gapped Environment With Embedded PostgreSQL]({{< relref "#chef-automate-in-air-gapped-environment-with-embedded-postgresql" >}})
- [Chef Automate in Air-Gapped Environment With External PostgreSQL]({{< relref "#chef-automate-in-air-gapped-environment-with-external-postgresql" >}})

{{< note >}}

If you are unsure if your installation of Chef Automate is using an external PostgreSQL database, run `chef-automate config show`. You are using an external PostgreSQL database if `enable=true` is present in the `global.v1.external.postgresql` config setting.

{{< /note >}}

{{< warning >}}

Sixty percent of your drive should be free space before starting a major version upgrade.

{{< /warning >}}

### Chef Automate With Embedded PostgreSQL

To upgrade Chef Automate with embedded PostgreSQL, follow the steps given below:

**Upgrade Chef Automate**

1. If you haven't already done so, upgrade to the last date-based version of Chef Automate:

   ```sh
   sudo chef-automate upgrade run
   ```

   This should upgrade to Chef Automate version 20220329091442.

1. Check the availability of the latest version:

   ```sh
   sudo chef-automate upgrade status
   ```

1. Start a major version upgrade:

   ```sh
   sudo chef-automate upgrade run --major
   ```

   Once the upgrade is complete, you will get:

   - Information about the upgrade.
   - A checklist of requirements pre-upgrade.
   - A list of steps to perform post-upgrade.

**Upgrade PostgreSQL**

1. Check the upgrade status of Chef Automate:

   ```sh
   sudo chef-automate upgrade status
   ```

1. Migrate your data from PostgreSQL 9.6 to PostgreSQL 13:

   ```sh
   sudo chef-automate post-major-upgrade migrate --data=PG
   ```

1. Verify that all services are running:

   ```sh
   sudo chef-automate status
   ```

1. Verify that all the data is present in your upgraded Chef Automate version. If yes, clear the old PostgreSQL data:

   ```sh
   sudo chef-automate post-major-upgrade clear-data --data=PG
   ```

### Chef Automate With External PostgreSQL

To upgrade Chef Automate with external PostgreSQL, follow the steps given below:

**Upgrade Chef Automate**

1. If you haven't already done so, upgrade to the last date-based version of Chef Automate:

   ```sh
   sudo chef-automate upgrade run
   ```

   This should upgrade to Chef Automate version 20220329091442.

1. Check the availability of the latest version:

   ```sh
   sudo chef-automate upgrade status
   ```

1. Initiate a major version upgrade:

   ```sh
   sudo chef-automate upgrade run --major
   ```

   Once the upgrade is complete, you will get:

   - Information about the upgrade.
   - A checklist of requirements pre-upgrade.
   - A list of steps to perform post-upgrade.

**Upgrade External PostgreSQL**

1. Upgrade your external PostgreSQL database manually. See the [external PostgreSQL upgrade]({{< relref "postgres_external_upgrade.md" >}}) documentation. If you have configured *Host*, *Port*, or *Password* of PostgreSQL, patch the new configuration to use Chef Automate.

1. Verify that all services are running:

   ```sh
   sudo chef-automate status
   ```

### Chef Automate in Air-Gapped Environment With Embedded PostgreSQL

**Upgrade to Last Date-Based Version of Chef Automate**

To upgrade your version to **20220329091442** version, follow the steps given below:

1. Download the latest Chef Automate CLI to an internet-connected machine:

   ```sh
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
   ```

1. Create an Airgap Installation Bundle (AIB) using the internet-connected host:

   ```sh
   sudo ./chef-automate airgap bundle create --version 20220329091442
   ```

1. Copy the Chef Automate CLI (`chef-automate`) and the AIB (`automate_v20220329091442.aib`) to the air-gapped machine where you are running Chef Automate.

1. Run **Upgrade** in air-gapped machine:

   ```sh
   sudo ./chef-automate upgrade run --airgap-bundle /<PATH>/automate_v20220329091442.aib
   ```

1. Once done, confirm the status of the upgrade:

   ```sh
   chef-automate upgrade status
   ```

**Upgrade to Chef Automate 3.x.x**

Once you have upgraded to the last date-based version of Chef Automate, you can perform the major upgrade. To upgrade to 3.x.x, follow the steps below:

1. Create an Airgap Installation Bundle (AIB) on the internet-connected host:

   ```sh
   sudo ./chef-automate airgap bundle create --version 3.x.x
   ```

1. Copy the Chef Automate CLI (`chef-automate`) and AIB (`automate_v3.x.x.aib`) to the air-gapped machine running Chef Automate.

1. Make sure your upgrade-strategy is none

1. Upgrade Chef Automate on the air-gapped machine:

   ```sh
   sudo ./chef-automate upgrade run --airgap-bundle /<PATH>/automate_v3.x.x.aib --major
   ```

   Once the Chef Automate has upgraded, you will get:

   - Information about the upgrade.
   - A checklist of requirements pre-upgrade.
   - A list of steps to perform post-upgrade.

1. Check the status of the upgrade:

   ```sh
   chef-automate upgrade status
   ```

**Upgrade PostgreSQL**

1. Migrate your data from PostgreSQL 9.6 to PostgreSQL 13:

   ```sh
   chef-automate post-major-upgrade migrate --data=PG
   ```

1. Check whether all the services are running:

   ```sh
   chef-automate status
   ```

1. Verify that all the data is present in your upgraded Chef Automate. If yes, clear the old PostgreSQL data:

   ```sh
   chef-automate post-major-upgrade clear-data --data=PG
   ```

### Chef Automate in Air-Gapped Environment With External PostgreSQL

**Upgrade to Last Date-Based Version of Chef Automate**

1. Download the latest Chef Automate CLI to an internet-connected machine:

   ```sh
   curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
   ```

1. Create an Airgap Installation Bundle (AIB) using the internet-connected host:

   ```sh
   sudo ./chef-automate airgap bundle create --version 20220329091442
   ```

1. Copy the Chef Automate CLI (`chef-automate`) and the AIB (`automate_v20220329091442.aib`) to the air-gapped machine where you are running Chef Automate.

1. Upgrade the air-gapped machine:

   ```sh
   sudo ./chef-automate upgrade run --airgap-bundle /<PATH>/automate_v20220329091442.aib
   ```

1. Once done, confirm the status of the upgrade:

   ```sh
   sudo chef-automate upgrade status
   ```

**Upgrade to Chef Automate 3.x.x**

Once you have upgraded to the last date-based version of Chef Automate, you can perform the major upgrade. To upgrade to 3.x.x, follow the steps below:

1. Create an Airgap Installation Bundle (AIB) on the internet-connected host:

   ```sh
   sudo ./chef-automate airgap bundle create --version 3.x.x
   ```

1. Copy the Chef Automate CLI (`chef-automate`) and AIB (`automate_v3.x.x.aib`) to the air-gapped machine running Chef Automate.

1. Make sure your upgrade-strategy is none

1. Upgrade Chef Automate on the air-gapped machine:

   ```sh
   sudo ./chef-automate upgrade run --airgap-bundle /<PATH>/automate_v3.x.x.aib --major
   ```

   Once the Chef Automate has upgraded, you will get:

   - Information about the upgrade.
   - A checklist of requirements pre-upgrade.
   - A list of steps to perform post-upgrade.

1. Check the status of the upgrade:

   ```sh
   sudo chef-automate upgrade status
   ```

**Upgrade External PostgreSQL**

1. Upgrade your external PostgreSQL database manually. See the [external PostgreSQL upgrade]({{< relref "postgres_external_upgrade.md" >}}) documentation. If you have configured *Host*, *Port*, or *Password* of PostgreSQL, patch the new configuration to use Chef Automate.

1. Verify that all services are running:

   ```sh
   sudo chef-automate status
   ```

## Troubleshooting

If Chef Automate fails to upgrade your database for PostgreSQL 13 when running `chef-automate post-major-upgrade migrate --data=PG`, restore the data:

```sh
sudo chef-automate backup restore </path/to/backups/>BACKUP_ID
```

Use the backup ID from the backup you created before starting the upgrade.

If the restore fails even after upgrading the Chef Automate version, follow the steps given below:

1. Uninstall Chef Automate:

   ```sh
   sudo chef-automate uninstall
   ```

1. Install the last date-based version (`20220329091442`) using the air-gapped installation process. See the [air-gapped installation](/automate/airgapped_installation/) documentation for more information.

1. Restore the backup:

   ```sh
   sudo chef-automate backup restore <backup_id>
   ```

   See the [Chef Automate restore](/automate/restore/) documentation for more information.

{{< note >}}

If you have migrated database using alternative methods, you will see a checklist of steps after running `chef-automate upgrade status` which won’t be marked as complete. To remove those checklist items, run `sudo rm /hab/svc/deployment-service/var/upgrade_metadata.json`.

{{< /note >}}
