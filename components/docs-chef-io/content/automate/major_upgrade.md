+++
title = "Upgrade Chef Automate to 3.x.x"
date = 2022-03-03T12:02:46-08:00
draft = false

[menu]
  [menu.automate]
    title = "Upgrade to 3.x.x"
    identifier = "automate/major_upgrade.md Major Upgrade"
    parent = "automate"
+++

[\[edit on GitHub\]](https://github.com/chef/automate/blob/main/components/docs-chef-io/content/automate/major_upgrade.md)

Chef Automate is a platform that provides an entire suite of enterprise capabilities for node visibility and compliance. Chef Automate upgrades from one minor version to another automatically, but you cannot directly upgrade to any major version of Chef Automate. Use the `--major` flag to upgrade from the latest to a major version. This section will talk about the major version upgrade of Chef Automate with its possible scenarios.

## Prerequisites

* **Plan your downtime**: This upgrade comes with downtime. So before upgrading, set the environment to handle the downtime.
* **Automate Backup**: This Chef Automate version upgraded **PostgreSQL** version. So, take necessary backup of the data to help if there is a system failure.

## Upgrade Path

In this version, Chef Automate upgrades the embedded Postgres database from **v9.6** to **v13**. Chef Automate gets upgraded with four possible scenarios:

* [Automate with Embedded PostgreSQL]({{< relref "major_upgrade.md#customer-running-automate-with-embedded-pg" >}}).
* [Automate with External PostgreSQL]({{< relref "major_upgrade.md#customer-running-automate-with-external-pg" >}}).
* [Automate in Air Gapped Environment with Embedded PostgreSQL]({{< relref "major_upgrade.md#customer-running-automate-in-air-gapped-environment-with-embedded-pg" >}}).
* [Automate in Air Gapped Environment with External PostgreSQL]({{< relref "major_upgrade.md#customer-running-automate-in-air-gapped-environment-with-external-pg" >}})

{{< note >}} To upgrade the version of Chef Automate, upgrade your current version to the previous latest version. Run the command `chef-automate upgrade run` to upgrade the Chef Automate version. {{< /note >}}

### Automate with Embedded PostgreSQL

To upgrade Chef Automate with Embedded Postgres, follow the steps given below:

* To avoid blockers while migrating the data it is recommended to have almost 60% of disk space available before version upgrade.
* Check the availability of the latest version by using `chef-automate upgrade status` command.
* Run the `chef-automate upgrade run --major` command to start the upgrade process. Once the Chef Automate gets upgraded, you will get:
  * List of upgraded information.
  * Checklist of requirements pre-upgrade.
  * List of steps to perform post-upgrade.

* Check the status of the version upgrade using `$ chef-automate upgrade status` command.
* Migrate your data from PostgreSQL **v9.6** to **v13** by running `chef-automate post-major-upgrade migrate --data=PG` command.
* Check whether all the services are running using `chef-automate status` command.
* Rectify if all the data is present in your upgraded Chef Automate version. If yes, clear the old PostgreSQL data using `chef-automate post-major-upgrade clear-data --data=PG` command.

### Automate with External PostgreSQL

To upgrade Chef Automate with Embedded Postgres, follow the steps given below:

* Check the availability of the latest version by using `chef-automate upgrade status` command.
* Run the `chef-automate upgrade run --major` command to start the upgrade process. Once the Chef Automate gets upgraded, you will get:
  * List of upgraded information.
  * Checklist of requirements pre-upgrade.
  * List of steps to perform post-upgrade.

* Upgrade your **External PostgreSQL Database** manually using Database Administrator. Click [here]({{< relref "postgres_external_upgrade.md" >}}) to refer to the external postgres upgrade documentation. In case you have configured your *Host*, *Port* or *Password* of PostgreSQL, patch the new configuration to use Chef Automate.
* Check whether all the services are running using `chef-automate status` command. Once done, you are ready with your Chef Automate version upgrade.

### Automate in Air Gapped Environment with Embedded PostgreSQL

{{< note >}} To upgrade the version **(3.x.x)** of Chef Automate in Air Gapped Environment, firstly upgrade your version to the ongoing version **(20224354343234)**. Refer to the [Release Notes]({{< relref "release_notes_automate.md" >}}) to check the latest ongoing version. {{< /note >}}

#### Upgrade to Previously Released Version

To upgrade your version to **20224354343234** version, follow the steps given below:

* Download the latest CLI from Internet connected machine using `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip * > chef-automate && chmod +x chef-automate` command.
* Create AIB bundle using Internet connected machine using `chef-automate airgap bundle create /<path>/automate_v2022012400000.aib --version 20224354343234` command.
* Copy the CLI (chef-automate) and AIB (automate_v2022012400000.aib) to Air Gapped Machine where you are running the Chef Automate.
* Run **Upgrade** in Air Gapped Machine using `sudo chef-automate upgrade run --airgap-bundle /<path>/ automate_v2022012400000.aib` command.
* Once done, confirm the status upgrade using `$ chef-automate upgrade status` command.

#### Upgrade to 3.x.x

Once you have upgraded to the latest ongoing version, you can perform the significant upgrade. To upgrade to 3.x.x, follow the steps given below:

* Create AIB Bundle on Internet connected machine using `chef-automate airgap bundle create /<path>/automate_v22.0.1.aib --version 3.x.x` command.
* Copy the CLI (chef-automate) and AIB (automate_v22.0.1.aib) to Air Gapped Machine where you are running the Chef Automate.
* Run **Upgrade** in Air Gapped Machine using `sudo chef-automate upgrade run --airgap-bundle /<path>/automate_v3.x.x.aib --major` command. Once the Chef Automate gets upgraded you will get:
  * List of upgraded information.
  * Checklist of requirements pre-upgrade.
  * List of steps to perform post-upgrade.

* Check the status of the version upgrade using `$ chef-automate upgrade status` command.
* Migrate your data from PostgreSQL **v9.6** to **v13** by running `chef-automate post-major-upgrade migrate --data=PG` command.
* Check whether all the services are running using `chef-automate status` command.
* Rectify if all the data is present in your upgraded Chef Automate version. If yes, clear the old PostgreSQL data using `chef-automate post-major-upgrade clear-data --data=PG` command.

### Automate in Air Gapped Environment with External PostgreSQL

{{< note >}} To upgrade the version **(3.x.x)** of Chef Automate in Air Gapped Environment, firstly upgrade your version to the ongoing version **(20224354343234)**. Refer to the [Release Notes]({{< relref "release_notes_automate.md" >}}) to check the latest ongoing version. {{< /note >}}

#### Upgrade to Previously Released Version

To upgrade your version to **20224354343234** version, follow the steps given below:

* Download the latest CLI from Internet connected machine using `curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip * > chef-automate && chmod +x chef-automate` command.
* Create AIB bundle using Internet connected machine using `chef-automate airgap bundle create /<path>/automate_v2022012400000.aib --version 20224354343234` command.
* Copy the CLI (chef-automate) and AIB (automate_v2022012400000.aib) to Air Gapped Machine where you are running the Chef Automate.
* Run **Upgrade** in Air Gapped Machine using `sudo chef-automate upgrade run --airgap-bundle /<path>/ automate_v2022012400000.aib` command.
* Once done, confirm the status upgrade using `$ chef-automate upgrade status` command.

#### Upgrade to 3.x.x

Once you have upgraded to the latest ongoing version, you can perform the significant upgrade. To upgrade to 3.x.x, follow the steps given below:

* Create AIB Bundle on Internet connected machine using `chef-automate airgap bundle create /<path>/automate_v22.0.1.aib --version 3.x.x` command.
* Copy the CLI (chef-automate) and AIB (automate_v22.0.1.aib) to Air Gapped Machine where you are running the Chef Automate.
* Run **Upgrade** in Air Gapped Machine using `sudo chef-automate upgrade run --airgap-bundle /<path>/automate_v3.x.x.aib --major` command. Once the Chef Automate gets upgraded you will get:
  * List of upgraded information.
  * Checklist of requirements pre-upgrade.
  * List of steps to perform post-upgrade.

* Upgrade your **External PostgreSQL Database** manually using Database Administrator. Click [here]({{< relref "postgres_external_upgrade.md" >}}) to refer to the external postgres upgrade documentation. In case you have configured your *Host*, *Port* or *Password* of PostgreSQL, patch the new configuration to use Chef Automate.
* Check the status of the version upgrade using `$ chef-automate upgrade status` command. Once done, you are ready with your Chef Automate version upgrade.

## FAQ

How to check if automate is running external Postgres?

* Run `chef-automate config show` command. If `enable=true` is present in `global.v1.external.postgresql` it confirms that the automate is running external postgres.
