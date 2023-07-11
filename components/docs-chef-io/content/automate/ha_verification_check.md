+++
title = "Verification Check in Automate HA"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Verification Check in Automate HA"
    identifier = "automate/deploy_high_availability/ha_verification_check.md Verification Check in Automate HA"
    parent = "automate/deploy_high_availability"
    weight = 55
+++

{{< warning >}}
{{% automate/ha-warn %}}
{{< /warning >}}

**High Availability (HA)** is designed to avoid loss of service by reducing or managing failures and minimizing unscheduled downtime (when your system or network is not available for use or is unresponsive) that happens due to power outages or failure of a component. In this page, we will see how to reduce the possibility of errors and improve your overall CLI experience to deploy Automate HA. We will discuss the deployment of Automate HA by automatically checking verification tests in different Automate HA deployment workflows.

## Pre-requisites

Before you start, take a quick tour of our prerequisite pages for [On-Premises](/automate/ha_on_premises_deployment_prerequisites/) and [AWS deployment](/automate/ha_aws_deployment_prerequisites/).

## Configuration

Failures occuring during the deployment of Automate HA might result to clear up everything and the start the whole process once again. To avoid this, the following CLI has been introduced which verifies the Customer Managed Database.

```bash
chef-automate verify --config config.toml
```

The above command will run the `config.toml` file which contains .....

