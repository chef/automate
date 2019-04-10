+++
title = "Notifications"
description = "Notifications for Inspec compliance scans and Chef client runs."
date = 2018-05-18T13:19:02-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 10
+++

## About Notifications

Chef Automate notifications uses webhooks to send alerts on Chef client run and InSpec compliance scan failures. You can send notifications to Slack channels, Slack users, or any other service that supports webhook integrations.

By default only Admins of Chef Automate may create and manage notifications.

{{< warning >}}
Notifications sent by Chef Automate do not support retries; therefore, notifications sent while Slack or the external service receiving the notification is experiencing API issues, outages, or some other unplanned downtime may never be received. Undelivered notifications are not re-sent. Attempts to send notifications do generate log messages in your Chef Automate server.
{{< /warning >}}

## Slack Notifications

### Add a Slack Notification

To add a Slack notification for Chef Automate:

![Node Lifecycle](/images/docs/notifications-navigation.png)

1. In the **Settings** tab, navigate to the _Notifications_ page in the sidebar.
1. Select **Create Notification**.
1. Select **Slack**.
1. Enter a unique notification name.
1. Select the failure type to be notified on from the drop-down menu. Current options are chef client run or InSpec scan
1. Get your Slack webhook address by using the **What's this?** link, which opens an external Slack site.
1. On the Slack page, select a channel or user for the notification. Slack will create the new webhook and then provide a webhook URL for you to copy. After entering a recipient, use the **Add Incoming WebHooks Integration** button.
1. Copy the URL, return to the Chef Automate page, paste the URL into the _Notifications_ form.
1. Use the **Send a test** button to try out your Slack notification. If your Slack notification does not appear, return to the Slack Webhooks Integration page to re-check the recipient and URL.
1. Use the **Save Notification** button to create the Slack notification.

### Edit Slack Notifications

To edit a Slack notification for Chef Automate:

1. From the _Notifications_ page, select the notification name to open its detail page.
1. Edit the notification type, name, failure type, or URL.
1. Use the **Save Notification** button to save the Slack notification.

### Delete Slack Notifications

To delete a Slack notification for Chef Automate:

1. From the Notifications page, select **Delete** from the menu at the end of the table row.
1. Confirm that you wish to permanently delete this notification.

## Webhook Notifications

### Add a Webhook Notification

To add a webhook notification for Chef Automate:

![Node Lifecycle](/images/docs/notifications-navigation.png)

1. In the **Settings** tab navigate to the _Notifications_ page in the sidebar.
1. Select **Create Notification**.
1. Select **Webhooks**.
1. Enter a unique notification name.
1. Select the failure type to be notified on from the drop-down menu. Current options are chef client run or InSpec scan
1. Enter the webhook URL the notification should be sent to.
1. Use the **Send Test** button to try out your webhook notification.
1. Use the **Save Notification** button to create the webhook notification.

### Edit Webhook Notifications

To edit a webhook notification for Chef Automate:

1. From the _Notifications_ page, select the notification name to open its detail page.
1. Edit the notification type, name, failure type, or URL.
1. Use the **Save Notification** button to save the webhook notification.

### Delete Webhook Notifications

To delete a webhook notification for Chef Automate:

1. From the Notifications page, select **Delete** from the menu at the end of the table row.
1. Confirm that you wish to permanently delete this notification in the helper screen.
