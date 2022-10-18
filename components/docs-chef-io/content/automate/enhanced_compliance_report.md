+++
title = "Enhanced Compliance Report"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Enhanced Compliance Report"
    parent = "automate/configure"
    identifier = "automate/configure/enhanced_compliance_report.md Enhanced Compliance Report"
    weight = 80
+++

## What is Enhanced Compliance Reporting



## What is Assets Compliance Reporting



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



## Benchmark numbers on the time of upgrade based on the worker count



## Benchmark numbers on the ingestion process based on the worker count

