+++
title = "ServiceNow Event Creation App"

draft = true

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Event Creation App"
    identifier = "automate/integrations/servicenow/servicenow_event_creation.md Chef Automate Event Creation"
    parent = "automate/integrations/servicenow"
    weight = 20
+++

The Chef Automate Event Creation App is a ServiceNow-certified app available in the [ServiceNow](https://store.servicenow.com) store. The Event Creation App integrates Chef Automate with ServiceNow by enabling you to create events and alerts for failed Chef Infra client runs and compliance scans. This app captures the failures in your automated infrastructure and improves your event tracking and resolution.

The Event Creation App exposes the REST API endpoint for communication between Chef Automate and the ServiceNow instance. Chef Automate sends HTTPS JSON notifications to the Event Creation App in a ServiceNow instance to create and update events.

## User Requirements

- Your unique ServiceNow URL. It has the format: `https://ven12345.service-now.com`.
- Setting up the Event Creation App requires the `x_chef_automate.api` role in configuring it. Your ServiceNow administrator can enable this for you.

## Required ServiceNow Plugins

Install the `Event Management Core` plugin from the Service Management dashboard. Contact your ServiceNow administrator for more information.

## Install

Get the Event Creation App from the [ServiceNow](https://store.servicenow.com) store and then install it to your account from the **Service Management** dashboard.

{{< note >}}

 Refer [Event Creation App Roles]({{< relref "servicenow_reference/#event-creation-app-roles" >}}) section for information on event app roles.

{{< /note >}}

## Setup

You can set up automatic event creation and alerts for:

- Chef Infra client run alerts

  Includes the client run alerts grouped by the message key. The message key is either set as cookbook or node in the properties section.

- Chef InSpec scan alerts

  Includes the Chef Inspec scan run alerts grouped by the message key. The message key is either set as profile or node in the properties section.

- Chef Infra client runs

  Includes the client run records associated with the event created.

- Chef InSpec scans

  Includes the Inspec scan records associated with the events.

- Scheduled Jobs

  The scheduled jobs contain a script to delete the client runs and InSpec scans when the associated events are removed. Events are deleted by the event rules set by the clients.

  ![ServiceNow Event Creation modules](/images/automate/sn_event_menus.png)

## Properties

The Event Creation App has three configurable **Application Properties**. You must have the `admin` or `x_chef_event.admin` roles to change the default values in the **Application Properties**. Refer [Event Creation App Properties]({{< relref "servicenow_reference/#event-creation-app-properties" >}}) section for information on event app properties.

To change the Event Creation App properties:

1. Find **Chef Events** in ServiceNow.
1. Select **Chef Events** > **Properties** from the navigation.
1. Enter your changes in the **Chef Events Properties** form.
1. Select **Save**.

   ![ServiceNow Event Creation Configuration Page](/images/automate/sn_event_properties.png)

### Chef Infra Client Events

To set up automatic event reporting for failed Chef Infra Client runs from Chef Automate:

1. Confirm that you have the `x_chef_event.api` role.
1. Navigate to **Settings** > **Notifications**.
1. Select **Create Notification**.
1. Enter or select:
   - **Name**: A unique name for this notification.
   - **Webhook Type**: The product or proccess receiving the notification. Options: **Slack**, **Webhook**, and **ServiceNow**.
   - **Failure Type**: The Chef process for the notification. Select **Infra Client run failures** option.
   - **Webhook URL**: The API address that the notification uses. It has the format: `https://ven12345.service-now.com/api/x_chef_event/client_run`.
   - **ServiceNow Username**: The name you use to sign in to ServiceNow.
   - **ServiceNow Password**: The password you use to sign in to ServiceNow.
1. Select **Test Notification**. A successful test displays **Notification test connected successfully**. An unsuccessful test displays a message with information about any connection or credential problems to help you fix the error.

   ![Chef Automate Event Creation for Client Run Failures](/images/automate/sn_event_client_run_creation.png)

1. Select **Create Notification** to save this notification. A confirmation message, **Created notification "unique notification name specified"** appears.

### Chef InSpec Scan Events

Follow these steps to report failed Chef InSpec scans from Chef Automate:

1. Confirm that you have the `x_chef_event.api` role.
1. Navigate to **Settings** > **Notifications**.
1. Select **Create Notification**.
1. Enter or select:
   - **Name**: A unique name for this notification.
   - **Webhook Type**: The product or proccess receiving the notification. Options: **Slack**, **Webhook**, and **ServiceNow**.
   - **Failure Type**: The Chef process for the notification. Select the **Inspec compliance scan failures** option.
   - **Webhook URL**: The API address that the notification uses. It has the format:`https://ven12345.service-now.com/api/x_chef_event/inspec_scan`
   - **ServiceNow Username**: The name you use to sign in to ServiceNow.
   - **ServiceNow Password**: The password you use to sign in to ServiceNow.
1. Select **Test Notification**. A successful test displays **Notification test connected successfully**. An unsuccessful test displays a message with information about any connection or credential problems to help you fix the error.

   ![Chef Automate Notification for Chef InSpec Scan Failures](/images/automate/sn_event_compliance_scan_creation.png)

1. Select **Create Notification** to save this notification. A confirmation message, **Created notification "unique notification name specified"** appears.

## Uninstall

To uninstall the Event Creation App:

1. Navigate to the **System Applications** > **Applications** in ServiceNow.
1. Open the **Downloads** tab and select the **Chef Automate Event Creation**.
1. Navigate to **Related Links**.
1. Select **Uninstall**.
