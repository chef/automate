+++
title = "Upgrade to Chef Automate 3.x"
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

{{< warning >}} Existing A2 HA customers should NOT upgrade to 3.x now. Please wait for Chef Automate HA release. {{< /warning >}}

Chef Automate provides an entire suite of enterprise capabilities for node visibility and compliance. Chef Automate upgrades from one minor version to another automatically. However, Chef Automate will not automatically upgrade to a major version. See the instructions below for manually upgrading Chef Automate from date-based versions to Chef Automate 3.x.

## Upgrade Journey

Please choose following upgrade journey based on your current version of Chef Automate. All of these upgrades are manual upgrades.

| Your Current Version | Upgrade To |
| -------------------- | ---------- |
| Any version before 20220329091442| 20220329091442|
| 20220329091442| 3.0.x|

For example, if today you are on version 2021201164433, then your upgrade journey should be:

1. Manual upgrade to 20220329091442.
1. Manual upgrade to 3.0.x.

## Prerequisites

- **Plan your downtime**: This upgrade requires downtime. Before upgrading, set the environment to handle the downtime.
- **Backup Chef Automate database**: This Chef Automate version upgrades PostgreSQL. [Backup](/automate/backup/) your data before upgrading.
- **Current Version should be 20220329091442** If you are not on this version, please do normal upgrade as per your topology.

## Upgrade to version 20220329091442

Check your current version:

```sh
sudo chef-automate version
```

If your Server Version is less than 20220329091442.
Please upgrade to latest date pattern version number.
### Airgapped upgrade to 20220329091442

- On Internet connected machine
  - Download latest chef-automate cli.
  ```sh
  curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
  ```
  - Create bundle of version 20220329091442
  ```sh
  ./chef-automate airgap bundle create --version 20220329091442
  ```
  - Copy the bundle file `automate-20220329091442.aib` and latest downloaded CLI `chef-automate` to the airgapped machine running Chef Automate.
- On the airgapped machine running Chef Automate
  - Upgrade Automate with bundle
  ```sh
  sudo ./chef-automate upgrade run --airgap-bundle automate-20220329091442.aib
  ```

  - Check all services are up and running:

  ```sh
  sudo chef-automate status
  ```

### Normal Upgrade to 20220329091442

- Upgrade Chef Automate to latest minor version (20220329091442):

  ```sh
  sudo chef-automate upgrade run
  ```

- Check all services are up and running:

  ```sh
  sudo chef-automate status
  ```

## Upgrade Path to version 3.0.x from 20220329091442

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

**Upgrade Chef Automate from version 20220329091442 to 3.0.x**

1. Start a major version upgrade:\
    Here, you will be prompted to accept multiple Pre Upgrade checklist.
    Please ensure to do those actions before upgrade.
    ```sh
    sudo chef-automate upgrade run --major
    ```

Once the upgrade is complete, you will get a list of steps to perform post-upgrade.

**Post Upgrade Steps:**

1. Check the upgrade status of Chef Automate:

    ```sh
    sudo chef-automate upgrade status
    ```
    When the status is up-to-date, move ahead.

2. Migrate your data from PostgreSQL 9.6 to PostgreSQL 13:

    ```sh
    sudo chef-automate post-major-upgrade migrate --data=PG
    ```

3. Verify that all services are running:

    ```sh
    sudo chef-automate status
    ```

4. Verify that all the data is present in your upgraded Chef Automate. If yes, clear the old PostgreSQL 9.6 data:

    ```sh
    sudo chef-automate post-major-upgrade clear-data --data=PG
    ```

### Chef Automate With External PostgreSQL

To upgrade Chef Automate with external PostgreSQL, follow the steps given below:

**Upgrade Chef Automate from version 20220329091442 to 3.0.x**
1. Upgrade your external PostgreSQL database v9.6 to v13.5 manually. See the [external PostgreSQL upgrade]({{< relref "postgres_external_upgrade.md" >}}) documentation. If you have configured *Host*, *Port*, or *Password* of PostgreSQL, patch the new configuration to use Chef Automate.
3. Start major version upgrade:
    ```sh
    sudo chef-automate upgrade run --major
    ```
4. Check upgrade status is up-to-date
    ```sh
    sudo chef-automate status
    ```

### Chef Automate in Air-Gapped Environment With Embedded PostgreSQL

**Upgrade Chef Automate from version 20220329091442 to 3.0.x**

To upgrade to 3.0.x, follow the steps below:
#### On Internet connected machine:
1. Download latest CLI of Chef Automate
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

2. Create an Airgap Installation Bundle (AIB):

    ```sh
    sudo ./chef-automate airgap bundle create --version 3.0.49
    ```
    OR we can directly download via curl request
    ```sh
    curl https://packages.chef.io/airgap_bundle/current/automate/3.0.49.aib -o automate-3.0.49.aib
    ```
3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate_3.0.x.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate:
1. Make sure your upgrade strategy as none in Chef Automate config. Check using:
    ```sh
    sudo ./chef-automate config show
    ```
    Reference to change [upgrade strategy]({{< relref "install.md#disable-automatic-upgrades" >}})

2. Upgrade using new AIB and Chef Automate CLI:

    ```sh
    sudo ./chef-automate upgrade run --airgap-bundle automate_3.0.x.aib --major
    ```
**Post Upgrade Steps:**

1. Check the upgrade status of Chef Automate:

    ```sh
    sudo chef-automate upgrade status
    ```
    When the status is up-to-date, move ahead.

2. Migrate your data from PostgreSQL 9.6 to PostgreSQL 13:

    ```sh
    sudo chef-automate post-major-upgrade migrate --data=PG
    ```

3. Verify that all services are running:

    ```sh
    sudo chef-automate status
    ```

4. Verify that all the data is present in your upgraded Chef Automate. If yes, clear the old PostgreSQL 9.6 data:

    ```sh
    sudo chef-automate post-major-upgrade clear-data --data=PG
    ```

### Chef Automate in Air-Gapped Environment With External PostgreSQL

**Upgrade Chef Automate from version 20220329091442 to 3.0.x**

To upgrade to 3.0.x, follow the steps below:
#### On Internet connected machine:
1. Download latest CLI of Chef Automate
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

2. Create an Airgap Installation Bundle (AIB):

    ```sh
    sudo ./chef-automate airgap bundle create --version 3.0.49
    ```
    OR we can directly download via curl request
    ```sh
    curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate-3.0.49.aib
    ```

3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate_3.0.x.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate:
1. Make sure your upgrade strategy as none in Chef Automate config. Check using:
    ```sh
    sudo ./chef-automate config show
    ```
    Reference to change [upgrade strategy]({{< relref "install.md#disable-automatic-upgrades" >}})
2. Upgrade your external PostgreSQL database v9.6 to v13.5 manually. See the [external PostgreSQL upgrade]({{< relref "postgres_external_upgrade.md" >}}) documentation. If you have configured *Host*, *Port*, or *Password* of PostgreSQL, patch the new configuration to use Chef Automate.
3. Upgrade using new AIB and Chef Automate CLI:

    ```sh
    sudo ./chef-automate upgrade run --airgap-bundle automate_3.0.x.aib --major
    ```
4. Check upgrade status is up-to-date
    ```sh
    sudo chef-automate status
    ```

## Troubleshooting

If Chef Automate fails to migrate your data to PostgreSQL 13 when running `chef-automate post-major-upgrade migrate --data=PG`, restore the data:
```sh
sudo chef-automate backup restore </path/to/backups/>BACKUP_ID
```
If you have Air-Gapped bundle which you want to restore to, then use this command:
```sh
sudo chef-automate backup restore  --airgap-bundle </path/to/bundle> </path/to/backups/>BACKUP_ID
```
Reference for [Restore methods](/automate/restore/).

Use the backup ID from the backup you created before starting the upgrade.

If the restore fails even after upgrading the Chef Automate version, follow the steps given below:

1. Uninstall Chef Automate:
    ```sh
    sudo chef-automate uninstall
    ```
2. Install the last date-based version (`20220329091442`) using the air-gapped installation process. See the [air-gapped installation](/automate/airgapped_installation/) documentation for more information.

3. Restore the backup:

    ```sh
    sudo chef-automate backup restore <backup_id>
    ```

See the [Chef Automate restore](/automate/restore/) documentation for more information.

{{< note >}}

If you used backup and restore method to migrate data then you will have to remove this file: `/hab/svc/deployment-service/var/upgrade_metadata.json`. To mock completion of post upgrade checklist items.

{{< /note >}}
