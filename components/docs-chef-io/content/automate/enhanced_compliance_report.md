+++
title = "Enhanced Compliance Report Ingestion"

draft = true

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Enhanced Compliance Report Ingestion"
    parent = "automate/configure"
    identifier = "automate/configure/enhanced_compliance_report.md Enhanced Compliance Report"
    weight = 80
+++

**Enhanced Compliance Reporting** has been introduced in the Chef Automate which comes with a few new compliance APIs and modified compliance reporting APIs. The existing APIs can be used to work on a specific date. The new modification to the compliance APIs have been introduced to work on the Date Range.

While specifying the date range, the end date will always be the current date whereas the start date can be any date before 90 days.

The modification and introduction of compliance APIs cover the following functionalities:

- Finding the list of nodes over a date range.
- Finding the list of profiles used over a date range.
- Finding the list of controls across scans over a date range.
- Finding the statistical information of compliance scans across the nodes over a date range.

## Asset Compliance Reporting

Asset Compliance Reporting gives you count of how many compliance nodes have been reported between the specified date range. The new APIs cover the below functionalities:

- Finding the number of assets reported over a date range.
- Find the number of assets which have not reported over a date range.
- Find the number of assets which can be termed as **unreachable**.

The asset compliance report also gives the count of how many nodes have not been reported and have been unreachable in the specified date range.

The above-mentioned APIs are also data range specific, i.e., the end date will always be the current date whereas the start date can be any date before 90 days.

### Asset Reporting

Asset reporting can be differentiated into three types:

- **Reported Asset:** Any compliance node which has been sent or reported compliance between a specified data range time is called as **reported assets**.

- **Unreported Asset:** Any assets which do not send a report in a specified date range is called as **unreported assets**.

- **Unreachable Asset:** These assets are based on the predefined configuration. An asset is termed as unreachable if it has not sent any compliance report during the specified duration.

## Enable Enhanced and Assets Compliance Report

To enable the enhanced and asset compliance report:

1. Create a `patch.toml` if one does not already exist for your Chef Automate installation.

1. Add the following configuration to the `patch.toml` file:

    ```toml
    [compliance.v1.sys.service]

    enable_enhanced_compliance_reporting = true
    ```

    Here `enable_enhanced_compliance_reporting` attribute is set to **true**, which enables the enhanced and asset compliance report. To disable it, set the value of `enable_enhanced_compliance_reporting` to **false**.

1. Patch the config by running the following command:

    ```toml
    chef-automate config patch patch.toml
    ```

## Upgrade Status Command

Once you have enabled the enhanced compliance reporting (`enable_enhanced_compliance_reporting = true`), the data from the current indexes get migrated to the new indexes which are used to support the enhanced compliance reporting and asset compliance reporting.

{{< note >}} The migration process of the data from the existing indexes to the new indexes happens asynchronously. It takes limited resources but has the trade-off of taking a long time so that Automate system continues to behave as usual.{{< /note >}}

To check the status of the migration, run the following command:

```sh
chef-automate enhance-compliance migrate status
```

If the migration has completed the following message will be shown:

```sh
The Migration of compliance controls and assets have completed.
```

The migration is dependent on :

- The number of controls it needs to migrate across all the nodes
- The size of every compliance report totalling up the total size of reports
- The resource allocated to OpenSearch depending on the type of deployment

Here are some indications of migration performance:

| OpenSearch Deployment Type | Number of controls each report | Total size of report | Total time taken |
|----------------------------|--------------------------------|----------------------|------------------|
| External (8CPU, 32GB RAM)  | 2000                           | 6GB                  | 60 minutes          |
| Internal (4CPU, 16GB RAM)  | 2000                           | 6GB                  | 48 hours           |

## Compliance Data Ingestion and Impact

Compliance data ingestion ingests data from remote nodes using data-collector API.
The compliance data gets ingested to the OpenSearch indexes but also asynchronously loads data to the new indexes if `enhanced_compliance_reporting` is enabled.
These indexes are needed to support the APIs for `enhanced_compliance_reporting`.
The asynchronous data ingestion in to the new indexes are done using workers which are running at the background.

The number of asynchronous data populators can be configured by setting the following configuration:

```toml
[compliance.v1.sys.service]
    control_data_populators_count = 2
```

Here are some performance numbers executed on a machine with 4 vCPUs and 16 GB of RAM. The ingestion performance depends on:

- The number of CPU of the machine
- The size of memory of the machine
- The size of Compliance report
- The number of asynchronous control data populator.

| Size of the Report | Number of Nodes | Concurrency of Ingestion | Number of async Control Data Populators | Max CPU  | Max Memory |
|--------------------|-----------------|--------------------------|----------------------------------------|----------|------------|
| 1MB                | 5000             | 100                      | 2                                      | 94.5%    | 65.74%     |
| 1MB                | 5000             | 100                      | 5                                      | 94.3%    | 67.20%     |
| 3MB                | 5000             | 20                       | 2                                      | 95%      | 64%        |
| 3MB                | 5000             | 20                       | 5                                      | 99%      | 67%        |

## Performance Benchmark

The system gets impacted when `enhanced_compliance_reporting` is enabled. All the tests are performed on a minimum system requirement:

- 4 vCPUs
- 16 GB of RAM

{{< note >}} Please go through the numbers before enabling the **enhanced compliance reporting**.
It is always recommended using a dedicated machine for OpenSearch.{{< /note >}}

Please take note of the following performance test numbers before enabling the feature:

### Migration Impact on Ingestion

The migration of data to the new index is done in asynchronous manner slowly in the background.
The ingestion of data may be run during the migration and find out the impact on the resources.
The performance benchmark is executed on an embedded Automate installation running on a machine with 4 vCPUs and 16 GB of RAM.

| Enhanced Compliance Reporting enabled | Migration Running | Compliance Report Size | Concurrency | Max CPU Utilisation | Max Memory Utilisation |
|---------------------------------------|-------------------|------------------------|-------------|---------------------|------------------------|
| No                                    | No                | 3MB                    | 100         | 79%                 | 76%                    |
| Yes                                   | Yes               | 3MB                    | 100         | 97%                 | 78%                    |

The CPU utilization increment is primarily for OpenSearch service utilization.

### Disk Usage Impact Testing

The disk usage increases when all the data is migrated from the current indexes to the new indexes.
The following number depicts the disk size increase after completion of migration:

| Before Migration | After Migration | % of Increase |
|------------------|-----------------|---------------|
| 9.6 GB           | 13 GB           | 35%           |

### API Performance testing

The following tests are performed on a machine with 4 vCPUs and 16 GB of RAM by running:

- All the Compliance APIs called in parallel at certain duration

| Enhanced Compliance Reporting Enabled | No of Compliance APIs | Max CPU% | Min CPU % | Max Memory% | Min Memory % |
|---------------------------------------|-----------------------|----------|-----------|-------------|--------------|
| No                                    | 8                     | 69.01%   | 63.73%    | 67.37%      | 67.32%       |
| Yes                                   | 11                    | 70.28%   | 68.26%    | 67.68%      | 67.59%       |
