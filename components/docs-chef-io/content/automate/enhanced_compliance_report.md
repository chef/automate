+++
title = "Enhanced Compliance Report Ingestion"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Enhanced Compliance Report Ingestion"
    parent = "automate/configure"
    identifier = "automate/configure/enhanced_compliance_report.md Enhanced Compliance Report"
    weight = 80
+++

**Enhanced Compliance Reporting** has been introduced in the Chef Automate which comes with new compliance APIs. The existing APIs can be used to work on a specific date. The new introduced compliance APIs have been introduced to work on the Date Range.

While specifing the date range, the end date will always be the current date whereas the start date can be any date before 90 days.

```toml
APIs
```

## Asset Compliance Reporting

Asset Compliance Reporting gives you count of how many compliance nodes have been reported between the specified date range using the following API:

```toml
APIS
```

The asset compliance report also gives the count of how many nodes have not been reported and have been unreachable in the specified date range.

THe above mentioned APIs are also data range specific, i.e., the end date will always be the current date whereas the start date can be any date before 90 days.

### Asset Reporting

Asset reporting can be differentiated into three types:

* **Reported Asset:** Any compliance node which has been sent or reported compliance between a specified data range time is called as **reported assets**.

* **Unreported Asset:** Any assets which does not sent a report in a specified date range is called as **unreported assets**.

* **Unreachable Asset:** These assets are based on the predefined configuration. An asset is termed as unreachable if it has not sent any compliance report during the specificed duration.

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

{{< note >}} The migration process of the data from the existing indexes to the new indexes happens asynchronously. {{< /note >}}

To check the status of the migration, run the following command:

```sh
COMMAND
```

## Performance Benchmark

To check the performance benchmark, firstly set the value of `enable_enhanced_compliance_reporting` to `true`. This will enable the enhanced compliance reporting in your system. Enabling the enhanced compliance reporting will migrate your data from the current indexes to the new indexes. This migration takes place asynchronously at the back ground which does not disturbs the upgrade process and will not overload the data ingestion.

Once the enhanced compliance report is enabled and the ingestion has been started, let's take and example of a system with 2.3 GB data. Here the upgradation and the ingestion process is going on in parallel. Once the ingestion is done, the **Current CPU utilization** shows as **84%** whereas the **Current Memory Utilization** shows as **86.5%**. In the end, you Disk Usage can go up to **8.2GB**.

The performance benchmarking tests based on the assumptions are listed below:

TABLE FOR PERFORMANCE BENCHMARKING
