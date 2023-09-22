+++
title = "Config Verify"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Config Verify"
    identifier = "automate/deploy_high_availability/manage_ha_cluster/ha_verification_check.md Config Verify"
    parent = "automate/deploy_high_availability/manage_ha_cluster"
    weight = 230
+++

{{< note >}}
{{% automate/ha-warn %}}
{{< /note >}}

**High Availability (HA)** is designed to avoid loss of service by reducing or managing failures and minimizing unscheduled downtime (when your system or network is not available for use or is unresponsive) that happens due to power outages or failure of a component. In this page, we will see how to reduce the possibility of errors and improve your overall CLI experience to deploy Automate HA. We will discuss the deployment of Automate HA by automatically checking verification tests in different Automate HA deployment workflows.

## Pre-requisites

Before you start, take a quick tour of our prerequisite pages for [On-Premises](/automate/ha_on_premises_deployment_prerequisites/) and [AWS deployment](/automate/ha_aws_deployment_prerequisites/).

## Configuration

Failures occurring during the deployment of Automate HA might result to clear up everything and the start the whole process once again. To avoid this, the verify CLI has been introduced. You can run the CLI either pre or post deployment. The pre deployment CLI command is as follows:

```bash
chef-automate verify --config config.toml
```

The above command will trigger the `config.toml` file with config, the one you want to deploy.

The post deployment CLI command is as follows:

```bash
chef-automate verify
```

The above command will run verify check post deployment.

Once the verify command is triggered, following checks will be triggered:

- System Resources (All Nodes)
- Software Version (All Nodes)
- System User (All Nodes)
- External OpenSearch Database
- External PostgreSQL Database
- S3/MinIO Backup Config (If Required)
- NFS Backup Config (If Required)
- FQDN with Load Balancer Reachability
- Firewall Ports

You get a report irrespective of the fact that the everything passes of fails. If all the pointers in the report shows pass, it means everything is fine and you can move ahead with the Automate HA deployment.

## Benefits of Running the verify Command

### Pre-Deployment

It is always good to know things early. Pre deployment verification will give the potential pointers of failure which may occur at the time of deployment. The pre deployment verification will ensure certain checks in the system. These checks will ensure that the failure of critical services of the end points of the system are taken care of. For example, in case of a certificate check, it will ensure that the validation of the check has already happened before the deployment process.

### Post-Deployment

It's always better to know the critical scenarios of a deployment process before running it. But in case of a post deployment verification sometimes it can be beneficial as the deployment will help you to identify the proper state of the command.

## Reports

Once you run the verify command, it checks all the test cases defined. After it executes, you will see the full report of how many reports succeeded and how may failed. The report comes in a table structure with five columns, **NO**, **IDENTIFIER**, **PARAMETER**, **STATUS**, and **MESSAGE**. The MESSAGE column shows the pointers to resolve for the parameters which have failed.

An example of a checks performed are shown in the below image:

![Config Verify Check](/images/automate/verify_checks_example.png)

The above image shows the checks performed for Automate node. The checks in the above image are also performed on all the nodes in Automate HA, i.e., Chef Server, PostgreSQL, OpenSearch, and Chef Automate (the one showed above).

THe verify command checks all the nodes in Automate HA and with that it also provides the remediation steps for failures.
