+++
title = "Upgrade to Chef Automate 4.x"
date = 2022-03-03T12:02:46-08:00
draft = false
gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Upgrade to 4.x"
    identifier = "automate/upgrade/major_upgrade.md Major Upgrade 4.x"
    parent = "automate/upgrade"
    weight = 20
+++

Chef Automate provides an entire suite of enterprise capabilities for node visibility and compliance. Chef Automate upgrades from one minor version to another automatically. However, Chef Automatate will not automatically upgrade to a major version. See the instructions below for manually upgrading Chef Automate from date-based versions to Chef Automate 4.x.

## Upgrade Journey

Please choose following upgrade journey based on your current version of Chef Automate. All of these upgrades are manual upgrades.

| Your Current Version | Upgrade To |
| -------------------- | ---------- |
| Any version before 20220329091442| 20220329091442|
| 20220329091442| 3.0.x|
| 3.0.x| 4.0.x

For example, if today you are on version 2021201164433, then your upgrade journey should be:

1. Manual upgrade to 20220329091442.
1. Manual upgrade to 3.0.x.
1. Manual upgrade to 4.0.x

## Prerequisites

- **Plan your downtime**: This upgrade requires downtime. Before upgrading, set the environment to handle the downtime. Run `sudo chef-automate maintenance on` to turn on maintenance mode.
- **Backup Chef Automate database**: This Chef Automate version upgrades ElasticSearch. [Backup](/automate/backup/) your data before upgrading.
- **Current Version should be 3.0.x** If you are not on this version, please do normal upgrade as per your topology.

## Upgrade to version 3.0.x

Check your current version:
```sh
sudo chef-automate version
```
If your Server Version is less than 20220329091442, follow the link to [upgrade to 3.0.x]({{< relref "major_upgrade.md" >}})

If your Server Version is less than 3.0.x.
Please upgrade to latest minor version in 3 series.
### Airgapped upgrade to 3.0.x
- On Internet connected machine
  - Download latest chef-automate cli.
  ```sh
  curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
  ```
  - Create bundle of version 3.0.x
  ```sh
  ./chef-automate airgap bundle create --version 3.0.x
  ```
  - Copy this bundle file automate-3.0.x.aib and latest downloaded cli `chef-automate` to Automate Machine in airgap.
- On Automate Machine in Airgap
  - Upgrade Automate with bundle
  ```sh
  sudo ./chef-automate upgrade run --airgap-bundle automate-3.0.x.aib
  ```
  - Check all services are up and running
  ```sh
  sudo chef-automate status
  ```
### Normal Upgrade to 3.0.x
- Upgrade Chef Automate to latest minor version (3.0.x)

  ```sh
  sudo chef-automate upgrade run
  ```
- Check all services are up and running

  ```sh
  sudo chef-automate status
  ```


## Upgrade Path to version 4.0.x from 3.0.x

There are four possible upgrade scenarios:

- [Chef Automate with Embedded Elasticsearch]({{< relref "#chef-automate-with-embedded-elasticsearch" >}})
- [Chef Automate with External Elasticsearch]({{< relref "#chef-automate-with-external-elasticsearch" >}})
- [Chef Automate in Air-Gapped Environment With Embedded Elasticsearch]({{< relref "#chef-automate-in-air-gapped-environment-with-embedded-elasticsearch" >}})
- [Chef Automate in Air-Gapped Environment With External Elasticsearch]({{< relref "#chef-automate-in-air-gapped-environment-with-external-elasticsearch" >}})

{{< note >}}

If you are unsure if your installation of Chef Automate is using an external elasticsearch, run `chef-automate config show`. You are using an external elasticsearch if `enable=true` is present in the `global.v1.external.elasticsearch` config setting.

{{< /note >}}

{{< warning >}}

Sixty percent of your drive should be free space before starting a major version upgrade.

{{< /warning >}}

{{< warning >}}

Sharding needs to be disabled for automate running embedded elasticsearch. Please accept the checklist item asking permission to disable sharding.

{{< /warning >}}

### Chef Automate With Embedded Elasticsearch

To upgrade Chef Automate with embedded Elasticsearch, follow the steps given below:

**Upgrade Chef Automate from version 3.0.x to 4.0.x**

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

2. Turn off maintenance mode:

    ```sh
    sudo chef-automate maintenance off
    ```

3. Migrate your data from elasticsearch 6.8 to Opensearch 1.2.4:

    ```sh
    sudo chef-automate post-major-upgrade migrate --data=es
    ```

4. Verify that all services are running:

    ```sh
    sudo chef-automate status
    ```

5. Verify that all the data is present in your upgraded Chef Automate. If yes, clear the old elasticsearch 6.8 data:

    ```sh
    sudo chef-automate post-major-upgrade clear-data --data=es
    ```

### Chef Automate With External Elasticsearch

To upgrade Chef Automate with external elasticsearch, follow the steps given below:

**Upgrade Chef Automate from version 3.0.x to 4.0.x**
1. Upgrade your external elasticsearch 6.8 to opensearch 1.2.4 manually. If you have configured *Host*, *Port*, *Username* or *Password* of Elasticsearch, patch the new configuration to use Chef Automate.
2. Start major version upgrade:
    ```sh
    sudo chef-automate upgrade run --major
    ```
3. Check upgrade status is up-to-date
    ```sh
    sudo chef-automate status
    ```
4. Turn off maintenance mode:

    ```sh
    sudo chef-automate maintenance off
    ```

### Chef Automate in Air-Gapped Environment With Embedded Elasticsearch

**Upgrade Chef Automate from version 3.0.x to 4.0.x**

To upgrade to 4.0.x, follow the steps below:
#### On Internet connected machine:
1. Download latest CLI of Chef Automate
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

2. Create an Airgap Installation Bundle (AIB):

    ```sh
    sudo ./chef-automate airgap bundle create
    ```

3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate_4.0.x.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate:
1. Make sure your upgrade strategy as none in Chef Automate config. Check using:
    ```sh
    sudo ./chef-automate config show
    ```

2. Upgrade using new AIB and Chef Automate CLI:

    ```sh
    sudo ./chef-automate upgrade run --airgap-bundle automate_4.0.x.aib --major
    ```
**Post Upgrade Steps:**

1. Check the upgrade status of Chef Automate:

    ```sh
    sudo chef-automate upgrade status
    ```
    When the status is up-to-date, move ahead.

2. Turn off maintenance mode:

    ```sh
    sudo chef-automate maintenance off
    ```

3. Migrate your data from Elasticsearch 6.8 to Opensearch 1.2.4:

    ```sh
    sudo chef-automate post-major-upgrade migrate --data=es
    ```

4. Verify that all services are running:

    ```sh
    sudo chef-automate status
    ```

5. Verify that all the data is present in your upgraded Chef Automate. If yes, clear the old elasticsearch 6.8 data:

    ```sh
    sudo chef-automate post-major-upgrade clear-data --data=es
    ```

### Chef Automate in Air-Gapped Environment With External Elasticsearch

**Upgrade Chef Automate from version 3.0.x to 4.0.x**

To upgrade to 4.0.x, follow the steps below:
#### On Internet connected machine:
1. Download latest CLI of Chef Automate
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

2. Create an Airgap Installation Bundle (AIB):

    ```sh
    sudo ./chef-automate airgap bundle create
    ```

3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate_4.0.x.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate:
1. Make sure your upgrade strategy as none in Chef Automate config. Check using:
    ```sh
    sudo ./chef-automate config show
    ```
2. Upgrade your external Elasticsearch 6.8 to Opensearch 1.2.4 manually. If you have configured *Host*, *Port*, *Username* or *Password* of Elasticsearch, patch the new configuration to use Chef Automate.
3. Upgrade using new AIB and Chef Automate CLI:

    ```sh
    sudo ./chef-automate upgrade run --airgap-bundle automate_4.0.x.aib --major
    ```
4. Check upgrade status is up-to-date
    ```sh
    sudo chef-automate status
    ```

5. Turn off maintenance mode:

    ```sh
    sudo chef-automate maintenance off
    ```

## Troubleshooting

If Chef Automate fails to migrate your data to Opensearch 1.2.4 when running `chef-automate post-major-upgrade migrate --data=es`, restore the data:
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
2. Install the last Major version (`3.0.x`) using the air-gapped installation process. See the [air-gapped installation](/automate/airgapped_installation/) documentation for more information.

3. Restore the backup:

    ```sh
    sudo chef-automate backup restore <backup_id>
    ```

See the [Chef Automate restore](/automate/restore/) documentation for more information.

{{< note >}}

If you used backup and restore method to migrate data then you will have to remove this file: `/hab/svc/deployment-service/var/upgrade_metadata.json`. To mock completion of post upgrade checklist items.

{{< /note >}}
