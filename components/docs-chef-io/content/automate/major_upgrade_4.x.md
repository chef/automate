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
    weight = 30
+++

{{< warning >}}
{{% automate/4x-warn %}}
{{< /warning >}}

{{< warning >}} *Elasticsearch support has been removed from this version (4.0.27) of Chef Automate.* {{< /warning >}}

Chef Automate provides an entire suite of enterprise capabilities for node visibility and compliance. Chef Automate upgrades from one minor version to another automatically. However, Chef Automate will not automatically upgrade to a major version. See the instructions below for manually upgrading Chef Automate from date-based versions to Chef Automate *4.x*.

## Upgrade Journey

Please choose the following upgrade journey based on your current version of Chef Automate. All of these upgrades are manual upgrades.

| Your Current Version | Upgrade To |
| -------------------- | ---------- |
| Any version before 20220329091442| 20220329091442|
| 20220329091442| 3.0.49|
| 3.0.49| 4.0.x

For example, if today you are on version *2021201164433*, your upgrade journey should be:

1. Manual upgrade to *20220329091442*
1. Manual upgrade to *3.0.49*
1. Manual upgrade to *4.0.x*

## Prerequisites

{{< note >}} If your Elasticsearch contains older indexes of version 5, please [re-index](https://www.elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html) them to version 6 with the same name, before proceeding with the upgrade. {{< /note >}}

- **Plan your downtime:** This upgrade requires downtime. Before upgrading, set the environment to handle the downtime. Run `sudo chef-automate maintenance on` to turn on maintenance mode.
- **Backup Chef Automate database:** This Chef Automate version upgrades ElasticSearch. [Backup](/automate/backup/) your data before upgrading.
- **Current version should be 3.0.49:** If you are not on the *3.0.49* version, do regular upgrades according to your topology.

## Upgrade to Version 3.0.49

Check your current version:

```sh
sudo chef-automate version
```

{{< note >}} If your server version is less than *20220329091442*, refer to [Upgrade to 3.0.49]({{< relref "major_upgrade.md" >}}) page. {{< /note >}}

{{< note >}} If your server version is less than *3.0.49*, upgrade to the latest minor version in the 3 series. {{< /note >}}

### Normal and airgapped upgrade to 3.0.49

To upgrade to automate 3.0.49 follow the link: [upgrading to 3.0.49]({{< relref "major_upgrade.md" >}})

## Upgrade Path from 3.0.49 to 4.0.x

There are four possible scenarios to upgrade from the 3.0.49 to 4.0.x version.

- [Chef Automate with Embedded Elasticsearch]({{< relref "#chef-automate-with-embedded-elasticsearch" >}})

- [Chef Automate with External Elasticsearch]({{< relref "#chef-automate-with-external-elasticsearch" >}})

- [Chef Automate in Air-Gapped Environment With Embedded Elasticsearch]({{< relref "#chef-automate-in-air-gapped-environment-with-embedded-elasticsearch" >}})

- [Chef Automate in Air-Gapped Environment With External Elasticsearch]({{< relref "#chef-automate-in-air-gapped-environment-with-external-elasticsearch" >}})

{{< note >}} Confirm whether your installation is using an external Elasticsearch by running the `chef-automate config show` command. If `enable=true` is present in the `global.v1.external.elasticsearch` config setting, you are using a external Elasticsearch. {{< /note >}}

{{< warning >}} Your drive should have a minimum of sixty percent of free space to start the major version upgrade. {{< /warning >}}

{{< warning >}} Upgrade will disable the **sharding** for automate running embedded Elasticsearch.

So, accept the checklist item asking permission to disable sharding. {{< /warning >}}

{{< note >}} If your backup location is S3 and the endpoint is configured as regional, then change your settings as below:

```shell
[global.v1.backups]
  location = "s3"
[global.v1.backups.s3.bucket]
  # name (required): The name of the bucket
  name = "<bucket name>"

  # endpoint (required): The endpoint for the region the bucket lives in for Automate Version 3.x.y
  # endpoint (required): For Automate Version 4.x.y, use this https://s3.amazonaws.com
  endpoint = "https://s3.amazonaws.com"
```

{{< /note >}}

### Chef Automate With Embedded Elasticsearch

To upgrade Chef Automate with embedded Elasticsearch, follow the steps given below:

**Upgrade Chef Automate from version 3.0.49 to 4.0.x**

1. Start a major version upgrade:

```sh
sudo chef-automate upgrade run --major
```

Here, you will be prompted to accept multiple Pre Upgrade checklists. Ensure you have performed all those actions before confirming an upgrade. Otherwise, it will prompt you with an error. The checklist will be as follows:


```shell
You had planned for a downtime by running the command(chef-automate maintenance on)?: (y/n)
y
You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
y
Ensure destination directory (/hab/) is having min. 'X' GB free space ? (y/n)
y
This will disable Sharding on your elastic search (y/n)
y
{"acknowledged":true,"persistent":{"cluster":{"routing":{"allocation":{"enable":"primaries"}}}},"transient":{}}
{"_shards":{"total":60,"successful":30,"failed":0}}
After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
y

Post Upgrade Steps:
===================

1. Check the status of your upgrade using:  
     $ chef-automate upgrade status
  
2. Disable the maintenance mode if you enabled it previously using:
	$ chef-automate maintenance off
  
3. Migrate Data from Elastic Search to Open Search using this command:
     $ chef-automate post-major-upgrade migrate --data=es
  
4. Check Automate UI everything is running, and all data is visible
  
5. If you are sure all data is available in Upgraded Automate, then we can free up old elastic search Data by running: 
     $ chef-automate post-major-upgrade clear-data --data=es
**** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

Now, the upgrade will start. Please confirm to continue... (y/n)
y
```

{{< note >}} To continue with the upgradation, the /hab must and should contain at least 5GB of free space. {{< /note >}}

{{< note >}} The 'X' GB is the minimum free space required in /hab which is calculated with the formula => [ ((/hab size or /hab/svc/automate-elasticsearch/data) + 10%) or 5GB ] . {{< /note >}}

Once you are done with the upgrade. Follow the steps post upgrade, which are:

2. Check the upgrade status of Chef Automate:

```sh
sudo chef-automate upgrade status
```

The above command should return: Automate is up-to-date with airgap bundle `4.x.y` version

3. Turn off maintenance mode:

```sh
sudo chef-automate maintenance off
```

The above command should return:

```result
Updating deployment configuration
Applying deployment configuration
```

4. Migrate your data from *ElasticSearch 6.8* to *OpenSearch 1.2.4*:

```sh
sudo chef-automate post-major-upgrade migrate --data=es
```

```shell
It will start the migration immediately after checking.
Press y to agree, n to disagree? [y/n]: y

HAB Root Path /hab/pkgs/chef/deployment-service/0.1.0/20220609123606

----------------------------------------------
Chef-automate stop 
----------------------------------------------

Chef Automate Stopped

----------------------------------------------
migration from es to os 
----------------------------------------------

Checking for es_upgrade

Once done with the migration, please wait for some time to reindex the data

----------------------------------------------
Chef-automate start 
----------------------------------------------

Starting Chef Automate

----------------------------------------------
Chef-automate status 
----------------------------------------------
```

The default config will be shown once the migration is successful.

6. Verify whether all services are running:

```sh
sudo chef-automate status
```

7. Clear the old ElasticSearch 6.8 data if all the data is present in your upgraded Chef Automate.

```sh
sudo chef-automate post-major-upgrade clear-data --data=es
```

### Chef Automate with External ElasticSearch

To upgrade Chef Automate with external Elasticsearch, follow the steps given below:

**Upgrade Chef Automate from version 3.0.49 to 4.0.x**

1. Start major version upgrade:

```sh
sudo chef-automate upgrade run --major
```

Here, you will be prompted to accept multiple Pre Upgrade checklists. Ensure you have performed all those actions before the upgrade, then mark yes. Otherwise, it will prompt you with an error. The checklist will be as follows:

```shell
You had planned for a downtime, by running the command(chef-automate maintenance on)?: (y/n)
y
You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
y
You are ready with your external open search with migrated data from elastic search, this should be done with the help of your Administrator (y/n)
y
After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
y

Post Upgrade Steps:
===================

1. Check the status of your upgrade using:  
     $ chef-automate upgrade status
   This should return: Automate is up-to-date

2. Check all services are running using: 
     $ chef-automate status

3. Disable the maintenance mode if you enabled it previously using:
	$ chef-automate maintenance off

4. Check Automate UI everything is running, and all data is visible

**** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

Now, the upgrade will start. Please confirm to continue... (y/n)
y
```

Once you are done with the upgrade. Follow the steps post upgrade, which is:

```shell
Post Upgrade Steps:
===================

1. Check the status of your upgrade using:  
     $ chef-automate upgrade status
  
2. Disable the maintenance mode if you enabled it previously using:
	$ chef-automate maintenance off
```

2. Check upgrade status is up-to-date

```sh
sudo chef-automate upgrade status
```

The above commabd should return: Automate is up-to-date.

3. manually upgrade your external *ElasticSearch 6.8* to *OpenSearch 1.2.4*. If you have configured *Host*, *Port*, *Username*, or *Password* of ElasticSearch, patch the new configuration to use Chef Automate.

4. Turn off maintenance mode:

```sh
sudo chef-automate maintenance off
```

Thr above command should return:
  
```result
Updating deployment configuration
Applying deployment configuration
```

### Chef Automate in Air-Gapped Environment With Embedded ElasticSearch

**Upgrade Chef Automate from version 3.0.49 to 4.0.x**

To upgrade to 4.0.x, follow the steps below:

#### On Internet connected machine

1. Download the latest CLI of Chef Automate.

```sh
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

2. Create an Airgap Installation Bundle (AIB).

```sh
sudo ./chef-automate airgap bundle create 
```

OR we can directly download via a curl request

```sh
curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate-4.x.y.aib
```

3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate-4.x.y.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate

1. Set your upgrade strategy to none in the Chef Automate config. Confirm by the following code:

```sh
sudo ./chef-automate config show
```

2. Upgrade using new AIB and Chef Automate CLI:

```sh
sudo ./chef-automate upgrade run --airgap-bundle automate-4.x.y.aib --major
```

Here, you will be prompted to accept multiple Pre Upgrade checklists. Ensure you have performed all those actions before the upgrade, then mark yes. Otherwise, it will prompt you with an error. The checklist will be as follows:

```shell
You had planned for a downtime by running the command(chef-automate maintenance on)?: (y/n)
y
You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
y
Ensure you have more than 'X' GB percent free disk space (y/n)
y
This will disable Sharding on your elastic search (y/n)
y
{"acknowledged":true,"persistent":{"cluster":{"routing":{"allocation":{"enable":"primaries"}}}},"transient":{}}
{"_shards":{"total":60,"successful":30,"failed":0}}
After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
y

Post Upgrade Steps:
===================

1. Check the status of your upgrade using:  
     $ chef-automate upgrade status
  
2. Disable the maintenance mode if you enabled it previously using:
  $ chef-automate maintenance off
  
3. Migrate Data from Elastic Search to Open Search using this command:
     $ chef-automate post-major-upgrade migrate --data=es
  
4. Check Automate UI everything is running, and all data is visible
  
5. If you are sure all data is available in Upgraded Automate, then we can free up old elastic search Data by running: 
     $ chef-automate post-major-upgrade clear-data --data=es
**** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

Now, the upgrade will start. Please confirm to continue... (y/n)
y
```

{{< note >}} To continue with the upgradation, the /hab must and should contain at least 5GB free space. {{< /note >}}

{{< note >}} The 'X' GB is the minimum free space required in /hab which is calculated with the formula => [ ((/hab size or /hab/svc/automate-elasticsearch/data) + 10%) or 5GB ] . {{< /note >}}

Once you are done with the upgrade. Follow the steps post upgrade, which are:

**Post Upgrade**

1. Check the upgrade status of Chef Automate, run the following command:

```sh
sudo chef-automate upgrade status
```

The above command should return: Automate is up-to-date with airgap bundle `4.x.y` version

2. Turn off the maintenance mode.

```sh
sudo chef-automate maintenance off
```

The above command should return:
  
  ```result
  Updating deployment configuration 
  Applying deployment configuration
  ```

3. Migrate your data from *ElasticSearch 6.8* to *OpenSearch 1.2.4*:

```sh
sudo chef-automate post-major-upgrade migrate --data=es
```

```shell
It will start the migration immediately after check.
Press y to agree, n to disagree? [y/n]: y

HAB Root Path /hab/pkgs/chef/deployment-service/0.1.0/20220609123606

----------------------------------------------
Chef-automate stop 
----------------------------------------------

Chef Automate Stopped

----------------------------------------------
migration from es to os 
----------------------------------------------

Checking for es_upgrade

Done with Migration 
 Please wait for some time to reindex the data

----------------------------------------------
Chef-automate start 
----------------------------------------------

Starting Chef Automate

----------------------------------------------
Chef-automate status 
----------------------------------------------
```

When migration is successful the default config will be shown

4. Verify whether all services are running:

```sh
sudo chef-automate status
```

5. Clear the old ElasticSearch 6.8 data if all the data is present in your upgraded Chef Automate.

```sh
sudo chef-automate post-major-upgrade clear-data --data=es
```

### Chef Automate in Air-Gapped Environment With External ElasticSearch

**Upgrade Chef Automate from version 3.0.49 to 4.0.x**

To upgrade to 4.0.x, follow the steps below:

#### On Internet connected machine

1. Download latest CLI of Chef Automate

```sh
curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
```

2. Create an Airgap Installation Bundle (AIB):

```sh
sudo ./chef-automate airgap bundle create
```

OR we can directly download via curl request

```sh
curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate-4.x.y.aib
```

3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate-4.x.y.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate

1. Set your upgrade strategy to none in Chef Automate config. Confirm by the following code:

```sh
sudo ./chef-automate config show
```

2. Upgrade using new AIB and Chef Automate CLI:

```sh
sudo ./chef-automate upgrade run --airgap-bundle automate-4.x.y.aib --major
```

Here, you will be prompted to accept multiple Pre Upgrade checklist. Ensure you have perfomed all those actions before upgrade then mark yes, otherwise it will prompt you the error.The checklist will be as following:

```shell
You had planned for a downtime, by running the command(chef-automate maintenance on)?: (y/n)
y
You have taken backup of your data and kept it safe, preferred on other disk or location? (y/n)
y
You are ready with your external open search with migrated data from elastic search, this should be done with the help of your Administrator (y/n)
y
After this upgrade completes, you will have to run Post upgrade steps to ensure your data is migrated and your Automate is ready for use (y/n)
y

Post Upgrade Steps:
===================

1. Check the status of your upgrade using:  
     $ chef-automate upgrade status
   This should return: Automate is up-to-date

2. Check all services are running using: 
     $ chef-automate status

3. Disable the maintenance mode if you enabled previously using:
	$ chef-automate maintenance off

4. Check Automate UI everything is running and all data is visible

**** In case of any errors, please refer to docs.chef.io and release notes for this version. ****

Now, upgrade will start, Please confirm to continue... (y/n)
y
```

It starts upgrading once you are done with the upgrade, follow the steps post upgrade which are:

```shell
Post Upgrade Steps:
===================

1. Check the status of your upgrade using:  
     $ chef-automate upgrade status
   This should return: Automate is up-to-date
  
2. Disable the maintenance mode if you enabled previously using:
	$ chef-automate maintenance off

3. Check whether upgrade status is up-to-date
```

```sh
sudo chef-automate upgrade status
```

4. Upgrade your external *ElasticSearch 6.8* to *OpenSearch 1.2.4* manually. If you have configured *Host*, *Port*, *Username* or *Password* of ElasticSearch, patch the new configuration to use Chef Automate.

5. Turn off maintenance mode using following command:

```sh
sudo chef-automate maintenance off
```

This should return:
  
  ```result
  Updating deployment configuration
  Applying deployment configuration
  ```

{{< note >}}
After upgrading to version 4.x, Automate will have the configurations both for OpenSearch and Elasticsearch. It is recommended to remove the Elasticsearch configuration after upgrading to External OpenSearch.
{{< /note >}}

## Troubleshooting

### Disc Space Validation

1. When /hab is less than 5gb the upgrade will fail with the below error:

```sh
Hab (/hab/) directory should have more than 5.00GB free space
UnknownError: An unknown issue occurred during execution: Request to start upgrade failed: one of the checklist was not accepted/satisfied for upgrade: Hab (/hab/) directory should have more than 5.00GB free space.
```

You need to clear the space in /hab for having atleast 5GB free space.

2. When the disk free space is less than the minimum required, the upgrade will fail with the below error:

```sh
UnknownError: An unknown issue occurred during execution: Request to start upgrade failed: one of the checklist was not accepted/satisfied for upgrade: You do not have minimum space available to continue with this upgrade. 
```

To skip this free disk space check please use ```--skip-storage-check``` flag (or)
To change destination directory please use ```--os-dest-data-dir``` flag

### Circuit Breaking Exception

```sh
{"error":{"root_cause":[{"type":"circuit_breaking_exception","reason":"[parent] Data too large, data for [<http_request>] would be [6126524880/5.7gb], which is larger than the limit of [5988548608/5.5gb], real usage: [6126524880/5.7gb], new bytes reserved: [0/0b], usages [request=0/0b, fielddata=74975/73.2kb, in_flight_requests=0/0b, accounting=89882860/85.7mb]","bytes_wanted":6126524880,"bytes_limit":5988548608,"durability":"PERMANENT"}]
```

- Update the Opensearch Config, using `chef-automate config patch <config_patch.toml>`

```sh
[opensearch]
  [opensearch.v1]
    [opensearch.v1.sys]
      [opensearch.v1.sys.runtime]
         heapsize = "8g" # This should be Half of RAM
      [opensearch.v1.sys.indices]
        [opensearch.v1.sys.indices.breaker]
          total_limit = "95%"
```

### Shard Failure

```sh
[ERROR] Elasticsearch exception [type=validation_exception, reason=Validation Failed: 1: this action would add [5] total shards, but this cluster currently has [997]/[1000] maximum shards open;]
```

To address the issue of shard limit hitting 1000, we need to increase the `max_shards_per_node`
Update the Opensearch Config, using `chef-automate config patch <config_patch.toml>`

```sh
[opensearch]
  [opensearch.v1]
    [opensearch.v1.sys]
      [opensearch.v1.sys.cluster]
         max_shards_per_node = "<NUMBER_OF_SHARD>"
```

### Migration Fails

1. When migration fails due to no minimum space available, the upgrade will fail with error:

```sh
Error while calDiskSizeAndDirSize : You do not have minimum space available to continue with this
```

To skip this free disk space check please use ```--skip-storage-check``` flag

2. If Chef Automate fails to migrate your data to *OpenSearch 1.2.4* while running `chef-automate post-major-upgrade migrate --data=es`, restore the data using:

```sh
sudo chef-automate backup restore </path/to/backups/>BACKUP_ID
```

To restore your Air-Gapped bundle, run the following command:

```sh
sudo chef-automate backup restore  --airgap-bundle </path/to/bundle> </path/to/backups/>BACKUP_ID
```

Click [here](/automate/restore/) to know more about the Restore methods.

To start the upgrade, use the **backup ID** from the backup created. In case the restore fails even after upgrading the Chef Automate version, follow the steps given below:

1. Uninstall Chef Automate.

```sh
sudo chef-automate uninstall
```

2. Install the last major version (`3.0.49`) using the [air-gapped installation](/automate/airgapped_installation/) process.

3. Restore the backup:

```sh
sudo chef-automate backup restore <backup_id>
```

Refer to the [Chef Automate Restore](/automate/restore/) documentation.

{{< note >}} Remove the `/hab/svc/deployment-service/var/upgrade_metadata.json` file if the migration of data has been done using backup and restore method. {{< /note >}}

### Minor upgrade errors

```sh
sudo chef-automate upgrade run --airgap-bundle x.x.x.aib
```

The above error occurs if you run Chef Automate with Proxy settings and upgrade it from the Automate version before 4.2.22 or after 4.2.22.

```sh
Installing airgap install bundle
DeploymentServiceCallError: A request to the deployment-service failed: Request to start to upgrade failed: RPC error: code = FailedPrecondition desc = The minimum compatible version field is missing in the manifest. Create a bundle with the latest automate-cli
```

This error may occur if a user running a non-airgapped version of Chef Automate tries to perform a minor upgrade using the airgapped installation method. To fix this minor upgrade error, run the following command:

```sh
sudo chef-automate stop
```

Once done, run the following command:

```sh
sudo chef-automate start
```

Before trying the upgrade again, confirm whether all the services are up by running the following command:

```sh
sudo chef-automate status
```
