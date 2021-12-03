+++
title = "ServiceNow Incident App"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Incident App"
    identifier = "automate/integrations/servicenow/servicenow_incident_creation.md Chef Automate Incident Creation"
    parent = "automate/integrations/servicenow"
    weight = 20
+++

The Chef Automate Event Creation App is a ServiceNow-certified app available in the [ServiceNow](https://store.servicenow.com) store. The Event Creation App integrates Chef Automate with ServiceNow. This app enables you to create events and alerts corresponding to the failed Chef Infra client runs and compliance scans. These events and alerts generated in their ServiceNow platforms due to the failures in the Chef Automate infrastructure automation updates and automated compliance checks. Thus, this app capture the failures in your automated infrastructure and improves your incident tracking and resolution.

The Event Creation App exposes the REST API endpoint for communication between Chef Automate and the ServiceNow instance. Chef Automate sends HTTPS JSON notifications to the Event Creation App in a ServiceNow instance to create and update events.

## User Requirements

- Your unique ServiceNow URL. It has the format: `https://ven12345.service-now.com`.
- Setting up the Event Creation App requires the `x_chef_automate.api` role to configure it. Your ServiceNow administrator can enable this for you.

## System Requirements

- The [Integration App]({{< relref "servicenow_integration" >}}) is already installed and configured.

## Required ServiceNow Plugins

Install `Event management Core` plugin from the Service Management dashboard. Kindly contact ServiceNow administrator for further details.

## Install

Get the Chef Automate Event Creation App from the [ServiceNow](https://store.servicenow.com) store and then install it to your account from the **Service Management** dashboard.

## Setup

You can setup automatic event creation and alerts for:

- Chef Infra client run alerts
- Chef InSpec scan alerts
- Chef Infra client runs
- Chef InSpec scans
- Scheduled Job

{{% servicenow_incidents %}} <dk>

## Properties

The Chef Automate Event Creation App has three configurable **Application Properties**. You must have the `admin` or `x_chef_incident.admin` roles to change the default values in the **Application Properties**.

To change the Event Creation App properties:

1. Find **Chef Events** in ServiceNow
1. Select **Chef Events** > **Properties** from the navigation.
1. Enter your changes in the **Chef Events Properties** form.
1. Select **Save**.

   ![ServiceNow Configuration Page](/images/automate/SNOW_config_page.png) <dk>

## Uninstall

To uninstall the Chef Automate Incident App:

1. Navigate to the **System Applications** > **Applications** in ServiceNow.
1. Open the **Downloads** tab and select the **Chef Automate Event Creation**.
1. Navigate to **Related Links**.
1. Select **Uninstall**.
