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
| 3.0.49| 4.x

For example, if today you are on version *2021201164433*, your upgrade journey should be:

1. Manual upgrade to *20220329091442*
1. Manual upgrade to *3.0.49*
1. Manual upgrade to *4.x*
 
## Upgrade and migration flow

![Upgrade flow](/images/automate/migration-and-upgrade-v4-flow.png)

## Prerequisites

{{< note >}} If your Elasticsearch contains older indexes of version 5, please [re-index](https://www.elastic.co/guide/en/elasticsearch/reference/6.8/docs-reindex.html) them to version 6 with the same name, before proceeding with the upgrade. {{< /note >}}

- **Plan your downtime:** This upgrade requires downtime. Before upgrading, set the environment to handle the downtime.
- **Backup Chef Automate database:** This Chef Automate version upgrades ElasticSearch. [Backup](/automate/backup/) your data before upgrading.
- **Current version should be 3.0.49:** If you are not on *3.0.49* version, do regular upgrades according to your topology.
- **Download latest chef-automate cli:**
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

## Upgrade to Version 3.0.49

Check your current version:

```sh
sudo chef-automate version
```

{{< note >}} If your server version is less than *20220329091442*, refer to [Upgrade to 3.0.49]({{< relref "major_upgrade.md" >}}) page. {{< /note >}}

{{< note >}} If your server version is less than *3.0.49*, upgrade to latest minor version in 3 series. {{< /note >}}

### Normal and airgapped upgrade to 3.0.49

To upgrade to automate 3.0.49 follow the link: [upgrading to 3.0.49]({{< relref "major_upgrade.md" >}})

## Upgrade Path from 3.0.49 to 4.x

There are four possible scenarios to upgrade from 3.0.49 to 4.x version.

- [Chef Automate with Embedded Elasticsearch]({{< relref "#chef-automate-with-embedded-elasticsearch" >}})

- [Chef Automate with External Elasticsearch]({{< relref "#chef-automate-with-external-elasticsearch" >}})

- [Chef Automate in Air-Gapped Environment With Embedded Elasticsearch]({{< relref "#chef-automate-in-air-gapped-environment-with-embedded-elasticsearch" >}})

- [Chef Automate in Air-Gapped Environment With External Elasticsearch]({{< relref "#chef-automate-in-air-gapped-environment-with-external-elasticsearch" >}})

{{< note >}} Confirm whether your installation is using an external Elasticsearch by running the `chef-automate config show` command. If `enable=true` is present in the `global.v1.external.elasticsearch` config setting, you are using a external Elasticsearch. 

If Automate is configured with external Elasticsearch then data should be migrated by user to external OpenSearch.

For Automate configured with embedded Elasticsearch then data will be migrate in the upgrade flow to embedded OpenSearch.
{{< /note >}}

{{< warning >}} Your drive should have a minimum of sixty percent of free space to start the major version upgrade. {{< /warning >}}

### Chef Automate With Embedded Elasticsearch

To upgrade Chef Automate with embedded Elasticsearch, follow the steps given below:

**Upgrade Chef Automate from version 3.0.49 to 4.x**

1. Download latest chef-automate cli.
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```
2. Start a major version upgrade:

    ```sh
    sudo ./chef-automate upgrade run --major
    ```


    ```shell
    #Output

    Current version: 3.0.49
    Target version: 4.x.y
    
    This is a major upgrade!
    In this release, Elasticsearch will be migrated to OpenSearch.
    
    Before proceeding, please ensure:
    1. You have scheduled downtime for the duration of the upgrade.
    2. You have taken a backup by running the command: chef automate backup create.
    3. The /hab directory should have at least 5.9GB of free space. (You have current available space : 39.6GB)
    
    You can always change the OpenSearch destination directory by using the flag:
      $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
    
    For more information, visit 
    https://docs.chef.io/automate/major_upgrade 4.x/
    
    Would you like to proceed with the upgrade? (y/n)
    y
    
    Following Pre-flight checks will be conducted
    1. The /hab directory should have at least 5.9GB of free space
    2. Elasticsearch indices are in version 6
    
    Pre flight checks
       [Passed]    The /hab directory should have at least 5.9GB of free space 
       [Passed]    Elasticsearch indices are in version 6 

       Maintenance mode turned ON successfully

    Upgrading Chef Automate from version 3.0.49 to 4.x
    This might take around 15 to 20 min
    
    Once upgrade is complete, You will get an option to migrate data from Elasticsearch to OpenSearch.
    Maintenance mode will be turned off after migration is complete.
    
    To check the upgrade status use $ chef-automate upgrade status
    ```

3. Check the upgrade status of Chef Automate:

    ```sh
    sudo chef-automate upgrade status
    ```
    

    ```shell
    #Output

    ------------------------------------------------------------------------------------
      Chef Automate upgraded to version: 4.x.y.
      Find out what's new in version (4.x.y) by visiting
      visit https://docs.chef.io/release_notes_automate/#4.x.y 
    ------------------------------------------------------------------------------------
    
    Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
    y
    
    Data Migration is in progress
              Maintenance mode turned ON successfully
              OpenSearch configurations updated successfully
              Chef Automate Stopped  
              Data Copied Successfully
              Chef Automate Started    
              Chef Automate status is healthy
              Maintenance mode turned OFF successfully
       Migration complete
    
    Verify Chef Automate to see that everything is running and that all your data is available.
    a2-dev.test #fqdn
    
    Once verified, you can remove old Elasticsearch data.
    
    Would you like to clean up the old Elasticsearch data now? (y/n)
    y
       Clean up successful  
    
    ```
4. **[If not done in previous steps]** Migrate your data from *ElasticSearch 6.8* to *OpenSearch 1.2.4*:

    ```sh
    sudo chef-automate post-major-upgrade migrate --data=es
    ```

    ```shell
    #Output

    Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
    y
    
    Data Migration is in progress
              Maintenance mode turned ON successfully
              OpenSearch configurations updated successfully
              Chef Automate Stopped  
              Data Copied Successfully
              Chef Automate Started    
              Chef Automate status is healthy
              Maintenance mode turned OFF successfully
       Migration complete
    
    Verify Chef Automate to see that everything is running and that all your data is available.
    ip-172-31-26-40.ap-south-1.compute.internal
    
    Once verified, you can remove old Elasticsearch data.
    
    
    ```


5. Verify whether all services are running:

    ```sh
    sudo chef-automate status
    ```

6. **[If not done in step 2]** Clear the old ElasticSearch 6.8 data if all the data is present in your upgraded Chef Automate.

    ```sh
    sudo chef-automate post-major-upgrade clear-data --data=es
    ```

    ```shell
    #Output

    Would you like to clean up the old Elasticsearch data now? (y/n)
    y
       Clean up successful 
    ```

### Chef Automate with External ElasticSearch

To upgrade Chef Automate with external Elasticsearch, follow the steps given below:

**Upgrade Chef Automate from version 3.0.49 to 4.x**

1. Download latest chef-automate cli.
    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

2. Start major version upgrade:

    ```sh
    sudo ./chef-automate upgrade run --major
    ```

    ```sh
    #Output

    Current version: 3.0.49
    Target version: 4.x.y
    
    This is a major upgrade!
    In this release, Elasticsearch will be migrated to OpenSearch.
    
    Before proceeding, please ensure:
    1. You have scheduled downtime for the duration of the upgrade.
    2. You have taken a backup by running the command: chef automate backup create.
    3. The /hab directory should have at least 5.9GB of free space. (You have current available space : 39.6GB)
    
    You can always change the OpenSearch destination directory by using the flag:
      $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
    
    For more information, visit 
    https://docs.chef.io/automate/major_upgrade 4.x/
    
    Would you like to proceed with the upgrade? (y/n)
    y
    
    Following Pre-flight checks will be conducted
    1. The /hab directory should have at least 5.9GB of free space
    
    Pre flight checks
       [Passed]    The /hab directory should have at least 5.9GB of free space 

       Maintenance mode turned ON successfully

    Upgrading Chef Automate from version 3.0.49 to 4.x
    ----------------------------------------------------------------------
    IMPORTANT 
    
    To establish connection between automate and OpenSearch database, 
    it is required to patch the configuration file with correct values.
    
    We have created a sample config file for configuring external OpenSearch:
    opensearch_config.toml
    
    Once upgrade is complete, you must update this file with actual external OpenSearch connection configurations 
    and then run the below patch command to update the configurations:
    $ chef-automate config patch opensearch_config.toml
    ----------------------------------------------------------------------

    To check the upgrade status use $ chef-automate upgrade status
    ```

3. Check upgrade status is up-to-date

    ```sh
    sudo chef-automate status
    ```

    
    ```shell
    #Output

    ------------------------------------------------------------------------------------
      Chef Automate upgraded to version: 4.x.y.
      Find out what's new in version (4.x.y) by visiting
      visit https://docs.chef.io/release_notes_automate/#4.x.y 
    ------------------------------------------------------------------------------------
    
    Have you updated your opensearch_config.toml with actual external OpenSearch connection configurations? (y/n)
    y
       External OpenSearch configurations updated successfully.

       Maintenance mode turned ON successfully

    After the upgrade, you must update opensearch.toml with actual external OpenSearch connection configurations 
    and then run the below patch command to update the configurations:
    $ chef-automate config patch opensearch.toml

      Maintenance mode turned ON successfully
    ```
4. Upgrade your external *ElasticSearch 6.8* to *OpenSearch 1.2.4* manually. If you have configured *Host*, *Port*, *Username* or *Password* of ElasticSearch, patch the new configuration to use Chef Automate.

5. All [relevant configuration fields](https://docs.chef.io/automate/opensearch/) of the Elasticsearch should be copied into the OpenSearch configuration.

    Please refer to the `elasticsearch.yml` file to get the applied configuration on your external Elasticsearch.
    Add the relevant configuration from external Elasticsearch (`elasticsearch.yml`) to the `opensearch.yml` on your external OpenSearch.

### Chef Automate in Air-Gapped Environment With Embedded ElasticSearch

**Upgrade Chef Automate from version 3.0.49 to 4.x**

To upgrade to 4.x, follow the steps below:

#### On Internet connected machine

1. Download latest CLI of Chef Automate.

    ```sh
    curl https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate
    ```

2. Create an Airgap Installation Bundle (AIB).

    ```sh
    sudo ./chef-automate airgap bundle create 
    ```
    OR we can directly download via curl request
    
    ```sh
    curl https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate-4.x.y.aib
    ```
3. Copy the latest Chef Automate CLI (`chef-automate`) and AIB (`automate-4.x.y.aib`) to the air-gapped machine running Chef Automate.

#### On Air-Gapped machine running Chef Automate

1. Make sure your upgrade strategy as none in Chef Automate config. Check using:
    ```sh
    sudo ./chef-automate config show
    ```
    Reference to change [upgrade strategy]({{< relref "install.md#disable-automatic-upgrades" >}})


2. Upgrade using new AIB and Chef Automate CLI:

    ```sh
    sudo ./chef-automate upgrade run --airgap-bundle automate-4.x.y.aib --major
    ```

    ```shell
    #Output

    Current version: 3.0.49
    Target version: 4.x.y
    
    This is a major upgrade!
    In this release, Elasticsearch will be migrated to OpenSearch.
    
    Before proceeding, please ensure:
    1. You have scheduled downtime for the duration of the upgrade.
    2. You have taken a backup by running the command: chef automate backup create.
    3. The /hab directory should have at least 5.9GB of free space. (You have current available space : 39.6GB)
    
    You can always change the OpenSearch destination directory by using the flag:
      $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
    
    For more information, visit 
    https://docs.chef.io/automate/major_upgrade 4.x/
    
    Would you like to proceed with the upgrade? (y/n)
    y
    
    Following Pre-flight checks will be conducted
    1. The /hab directory should have at least 5.9GB of free space
    2. Elasticsearch indices are in version 6
    
    Pre flight checks
       [Passed]    The /hab directory should have at least 5.9GB of free space 
       [Passed]    Elasticsearch indices are in version 6 

       Maintenance mode turned ON successfully

    Upgrading Chef Automate from version 3.0.49 to 4.x
    This might take around 15 to 20 min
    
    Once upgrade is complete, You will get an option to migrate data from Elasticsearch to OpenSearch.
    Maintenance mode will be turned off after migration is complete.
    
    To check the upgrade status use $ chef-automate upgrade status
    ```

3. Check the upgrade status of Chef Automate:

    ```sh
    sudo chef-automate upgrade status
    ```
    

    ```shell
    #Output

    ------------------------------------------------------------------------------------
     Chef Automate upgraded to airgap bundle version: 4.x.y.
      Find out what's new in version (4.x.y) by visiting
      visit https://docs.chef.io/release_notes_automate/#4.x.y 
    ------------------------------------------------------------------------------------
    
    Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
    y
    
    Data Migration is in progress
              Maintenance mode turned ON successfully
              OpenSearch configurations updated successfully
              Chef Automate Stopped  
              Data Copied Successfully
              Chef Automate Started    
              Chef Automate status is healthy
              Maintenance mode turned OFF successfully
       Migration complete
    
    Verify Chef Automate to see that everything is running and that all your data is available.
    a2-dev.test #fqdn
    
    Once verified, you can remove old Elasticsearch data.
    
    Would you like to clean up the old Elasticsearch data now? (y/n)
    y
       Clean up successful  
    
    ```
4. **[If not done in previous steps]** Migrate your data from *ElasticSearch 6.8* to *OpenSearch 1.2.4*:

    ```sh
    sudo chef-automate post-major-upgrade migrate --data=es
    ```

    ```shell
    #Output

    Do you wish to migrate the Elasticsearch data to OpenSearch now? (y/n)
    y
    
    Data Migration is in progress
              Maintenance mode turned ON successfully
              OpenSearch configurations updated successfully
              Chef Automate Stopped  
              Data Copied Successfully
              Chef Automate Started    
              Chef Automate status is healthy
              Maintenance mode turned OFF successfully
       Migration complete
    
    Verify Chef Automate to see that everything is running and that all your data is available.
    ip-172-31-26-40.ap-south-1.compute.internal
    
    Once verified, you can remove old Elasticsearch data.
    
    
    ```


5. Verify whether all services are running:

    ```sh
    sudo chef-automate status
    ```

6. **[If not done in step 2]** Clear the old ElasticSearch 6.8 data if all the data is present in your upgraded Chef Automate.

    ```sh
    sudo chef-automate post-major-upgrade clear-data --data=es
    ```

    ```shell
    #Output

    Would you like to clean up the old Elasticsearch data now? (y/n)
    y
       Clean up successful 
    ```

### Chef Automate in Air-Gapped Environment With External ElasticSearch

**Upgrade Chef Automate from version 3.0.49 to 4.x**

To upgrade to 4.x, follow the steps below:

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

1. Make sure your upgrade strategy as none in Chef Automate config. Check using:
    ```sh
    sudo ./chef-automate config show
    ```
    Reference to change [upgrade strategy]({{< relref "install.md#disable-automatic-upgrades" >}})
2. Upgrade using new AIB and Chef Automate CLI:

    ```sh
    sudo ./chef-automate upgrade run --airgap-bundle automate-4.x.y.aib --major
    ```

    ```sh
    #Output

    Current version: 3.0.49
    Target version: 4.x.y
    
    This is a major upgrade!
    In this release, Elasticsearch will be migrated to OpenSearch.
    
    Before proceeding, please ensure:
    1. You have scheduled downtime for the duration of the upgrade.
    2. You have taken a backup by running the command: chef automate backup create.
    3. The /hab directory should have at least 5.9GB of free space. (You have current available space : 39.6GB)
    
    You can always change the OpenSearch destination directory by using the flag:
      $ chef-automate upgrade run --major --os-dest-data-dir <path to new directory>
    
    For more information, visit 
    https://docs.chef.io/automate/major_upgrade 4.x/
    
    Would you like to proceed with the upgrade? (y/n)
    y
    
    Following Pre-flight checks will be conducted
    1. The /hab directory should have at least 5.9GB of free space
    
    Pre flight checks
       [Passed]    The /hab directory should have at least 5.9GB of free space 

       Maintenance mode turned ON successfully

    Upgrading Chef Automate from version 3.0.49 to 4.x
    ----------------------------------------------------------------------
    IMPORTANT 
    
    To establish connection between automate and OpenSearch database, 
    it is required to patch the configuration file with correct values.
    
    We have created a sample config file for configuring external OpenSearch:
    opensearch_config.toml
    
    Once upgrade is complete, you must update this file with actual external OpenSearch connection configurations 
    and then run the below patch command to update the configurations:
    $ chef-automate config patch opensearch_config.toml
    ----------------------------------------------------------------------

    To check the upgrade status use $ chef-automate upgrade status
    ```

3. Check upgrade status is up-to-date

    ```sh
    sudo chef-automate status
    ```

    
    ```shell
    #Output

    ------------------------------------------------------------------------------------
     Chef Automate upgraded to airgap bundle version: 4.x.y.
      Find out what's new in version (4.x.y) by visiting
      visit https://docs.chef.io/release_notes_automate/#4.x.y 
    ------------------------------------------------------------------------------------
    
    Have you updated your opensearch_config.toml with actual external OpenSearch connection configurations? (y/n)
    y
       External OpenSearch configurations updated successfully.

       Maintenance mode turned ON successfully

    After the upgrade, you must update opensearch.toml with actual external OpenSearch connection configurations 
    and then run the below patch command to update the configurations:
    $ chef-automate config patch opensearch.toml

      Maintenance mode turned ON successfully
    ```
4. Upgrade your external *ElasticSearch 6.8* to *OpenSearch 1.2.4* manually. If you have configured *Host*, *Port*, *Username* or *Password* of ElasticSearch, patch the new configuration to use Chef Automate.

5. All [relevant configuration fields](https://docs.chef.io/automate/opensearch/) of the Elasticsearch should be copied into the OpenSearch configuration.

    Please refer to the `elasticsearch.yml` file to get the applied configuration on your external Elasticsearch.
    Add the relevant configuration from external Elasticsearch (`elasticsearch.yml`) to the `opensearch.yml` on your external OpenSearch.


{{< note >}}
After upgrading to version 4.x, Automate will have the configurations both for OpenSearch and Elasticsearch. It is recommended to remove the Elasticsearch configuration after upgrading to External OpenSearch.
{{< /note >}}

## Troubleshooting

### Circuit Breaking Exception

```
{"error":{"root_cause":[{"type":"circuit_breaking_exception","reason":"[parent] Data too large, data for [<http_request>] would be [6126524880/5.7gb], which is larger than the limit of [5988548608/5.5gb], real usage: [6126524880/5.7gb], new bytes reserved: [0/0b], usages [request=0/0b, fielddata=74975/73.2kb, in_flight_requests=0/0b, accounting=89882860/85.7mb]","bytes_wanted":6126524880,"bytes_limit":5988548608,"durability":"PERMANENT"}]
```

- Update the Opensearch Config, using `chef-automate config patch <config_patch.toml>`
```
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

```
[ERROR] Elasticsearch exception [type=validation_exception, reason=Validation Failed: 1: this action would add [5] total shards, but this cluster currently has [997]/[1000] maximum shards open;]
```
To address the issue of shard limit hitting 1000, we need to increase the `max_shards_per_node`
Update the Opensearch Config, using `chef-automate config patch <config_patch.toml>` 
```
[opensearch]
  [opensearch.v1]
    [opensearch.v1.sys]
      [opensearch.v1.sys.cluster]
         max_shards_per_node = "<NUMBER_OF_SHARD>"
```
### Proxy Setting issue
If you are using Proxy Settings and have upgraded to a version between 4.0.27 and 4.2.10, then you might get this error when you upgrade:
```
DeploymentServiceCallError: A request to the deployment-service failed: Request to get upgrade status failed: rpc error: code = Unknown desc = error in getting the versions from current channel: error in invoking the endpoint https://packages.chef.io/manifests/current/automate/versions.json: Get "https://packages.chef.io/manifests/current/automate/versions.json": dial tcp: lookup packages.chef.io on 10.2.72.20:53: read udp 10.1.97.98:59620->10.2.72.20:53: i/o timeout
```
To move ahead with upgrade you can download latest CLI and Airgapped bundle using curl command with proxy settings:
```sh
curl -x http://proxy_server:proxy_port --proxy-user username:password -L https://packages.chef.io/files/current/latest/chef-automate-cli/chef-automate_linux_amd64.zip | gunzip - > chef-automate && chmod +x chef-automate

curl -x http://proxy_server:proxy_port --proxy-user username:password -L https://packages.chef.io/airgap_bundle/current/automate/latest.aib -o automate-latest.aib
```

After downloading, run the upgrade command with airgapped bundle option:
```sh
./chef-automate upgrade run --airgap-bundle automate-latest.aib
```
Output will look like this:
```sh
Installing airgap install bundle
Trying to restart Deployment Service...
Deployment service is stopped
Waiting for Deployment Service to be healthy
Waiting for Deployment Service to be healthy
Waiting for Deployment Service to be healthy
Deployment Service is healthy now
current_version:"4.x" target_version:"4.x.y" target_major:"4"
Upgrading Chef Automate
```

### Minor upgrade errors

```sh
sudo ./chef-automate upgrade run --airgap-bundle x.x.x.aib
```

The above error occurs if you run Chef Automate with Proxy settings and upgrade it from the Automate version before 4.2.22 to after 4.2.22.

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
### Migration Fails

 If Chef Automate fails to migrate your data to *OpenSearch 1.2.4* while running `chef-automate post-major-upgrade migrate --data=es`, restore the data using:

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

### Adding Custom Configuration to optimize OpenSearch performance
To add custom configurations or optimizing OpenSearch performance please refer to [Custom OpenSearch configuration docs](/automate/opensearch/#configure-embedded-opensearch).
