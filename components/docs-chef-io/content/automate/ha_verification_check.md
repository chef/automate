+++
title = "Config Verify"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Config Verify"
    identifier = "automate/deploy_high_availability/reference/ha_verification_check.md Config Verify"
    parent = "automate/deploy_high_availability/reference"
    weight = 250
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

The above command will trigger the `config.toml` file with config, the one you want to deploy. Once the verify command is triggered, following checks will be triggered:

- System Resources (All Nodes)
- Software Version (All Nodes)
- System User (All Nodes)
- External OpenSearch Database
- External PostgreSQL Database
- S3/Minio Backup Config (If Required)
- NFS Backup Config (If Required)
- FQDN with Load Balancer Reachability
- Firewall Ports

If all the above checks passes, you get a report. If all the pointers in the report shows pass, it means everything is fine and you can move ahead with the Automate HA deployment.

## Reports