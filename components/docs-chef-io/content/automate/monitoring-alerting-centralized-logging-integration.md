+++
title = "Chef Automate HA integrations"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Integrations"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/monitoring-alerting-centralized-logging-integration.md Chef Automate HA integrations"
    weight = 75
+++

You can integrate third-party platforms with [Chef Automate HA](/automate/ha) to monitor Automate's health, infrastructure, components, and services.

Chef Automate HA integrates with the following platforms:

- AWS CloudWatch
- Datadog
- Elastic Stack (ELK)
- Prometheus

## Prerequisites

These integrations are supported on Chef Automate HA 4.12.66 and above.

## AWS CloudWatch

Chef Automate HA with AWS CloudWatch gives automate and compliance teams the necessary redundancy and visibility to successfully operate the solution. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and CloudWatch Integration](https://github.com/chef/monitoring-integration-automate/blob/main/Whitepaper_AutomateHA_Monitoring_and_Alerting.md#cloudwatch-integration-with-automate-ha---monitoring)
- [Technical Blog reference of Automate HA and CloudWatch](https://www.chef.io/blog/monitoring-chef-automate-ha-with-cloudwatch)

## Datadog

Datadog is a monitoring and analytics platform that delivers real-time insights into application, infrastructure, and cloud service performance. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and DataDog Integration](https://github.com/chef/monitoring-integration-automate/blob/main/Whitepaper_AutomateHA_Monitoring_and_Alerting.md#datadog-integration-with-automate-ha---monitoring)
- [Technical Blog reference of Automate HA and DataDog](https://www.chef.io/blog/monitoring-chef-automate-ha-with-datadog)

## Elastic Stack

The ELK stack is a trio of open-source tools that have revolutionized how businesses harness their data. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and ELK Integration](https://github.com/chef/monitoring-integration-automate/blob/main/Whitepaper_AutomateHA_Monitoring_and_Alerting.md#elk-integration-with-automate-ha---centralised-logging)
- [Technical Blog reference of Automate HA and ELK](https://www.chef.io/blog/monitoring-chef-automate-ha-with-elk)

## Prometheus

Prometheus, an open-source tool excels at scraping metrics from HTTP endpoints, making it a go-to for harvesting data from various targets. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and Prometheus Integration](https://github.com/chef/monitoring-integration-automate/blob/main/Whitepaper_AutomateHA_Monitoring_and_Alerting.md#prometheus-integration-with-automate-ha---monitoring)
- [Technical Blog reference of Automate HA and Prometheus](https://www.chef.io/blog/monitoring-chef-automate-ha-with-prometheus)
