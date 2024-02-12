+++
title = "Monitoring Alerting and Centralized Logging Integration"

draft = false

gh_repo = "automate"
[menu]
  [menu.automate]
    title = "Monitoring Alerting and Centralized Logging Integration"
    parent = "automate/deploy_high_availability"
    identifier = "automate/deploy_high_availability/monitoring-alerting-centralized-logging-integration.md Monitoring Alerting and Centralized Logging Integration"
    weight = 75
+++

The Chef Automate HA equates to reliability, efficiency, and productivity built on Highly Available Infrastructure. To improve visibility into the health of the Chef-Automate HA infrastructure, its components, and its services one can integrate the Automate HA cluster with industry leading monitoring, alerting and logging platforms.​

The integration comes with:

- High uptime of critical Chef Automate HA infrastructure to maintain system stability

- Monitoring of Automate HA services​

- Providing advance notification for potential failures

- Enabling dashboard and reporting capabilities for Chef-Automate HA cluster

Integrations with the following monitoring, alerting, and centralized logging platforms are available:

## DataDog

Datadog is a monitoring and analytics platform that delivers real-time insights into application, infrastructure, and cloud service performance. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and DataDog Integration](https://github.com/chef/monitoring-integration-automate/tree/main/data-dog)

- [Technical Blog reference of Automate HA and DataDog](https://progresssoftware-my.sharepoint.com/:w:/g/personal/apravati_progress_com/Eb2zXjNHa71Brszy3V_SqsoB0LtyGP7eaBXtBlaqj9rGSA?e=gJcC8b)

## AWS CloudWatch

Chef Automate HA with AWS CloudWatch gives automate and compliance teams the necessary redundancy and visibility to successfully operate the solution. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and CloudWatch Integration](https://github.com/chef/monitoring-integration-automate/tree/main/cloud-watch)

- [Technical Blog reference of Automate HA and CloudWatch](https://progresssoftware-my.sharepoint.com/:w:/g/personal/apravati_progress_com/EQsURaeVai5Eszc9-3yiHWEBKCeDV0Nz-LhqC2giqefO-Q?e=KM0iWB)

## Prometheus

Prometheus, an open-source tool excels at scraping metrics from HTTP endpoints, making it a go-to for harvesting data from various targets. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and Prometheus Integration](https://github.com/chef/monitoring-integration-automate/tree/main/prometheus)

- [Technical Blog reference of Automate HA and Prometheus](https://progresssoftware-my.sharepoint.com/:w:/g/personal/apravati_progress_com/EZ23D4XeuKJGu-yMzmIehfwBPmTouhykSFqRhCJXdWSQWg?e=tysXun)

## ELK

The ELK stack is a trio of open-source tools that have revolutionized how businesses harness their data. Refer to the following links for the detailed whitepapers and blog documents.

- [Automate HA and ELK Integration](https://github.com/chef/monitoring-integration-automate/tree/main/ELK)

- [Technical Blog reference of Automate HA and ELK](https://progresssoftware-my.sharepoint.com/:w:/g/personal/apravati_progress_com/ERxHPHSM92RBnqxLL7GHlF8BNbdOzg_LuCHV-u4O4kH-pw?e=oB58sW)

{{< note >}} Chef Automate 4.12.x and above support integration with the above monitoring, alerting, and logging platforms. If your business requires assistance in setting this up or requires another platform, then please reach out to your customer success manager or account manager. {{< /note >}}
