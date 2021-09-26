### Chef Infra Client Incidents

To set up automatic incident reporting for failed Chef Infra Client runs from Chef Automate:

1. Confirm that you have the `x_chef_automate.api` role.
1. Navigate to **Settings** > **Notifications**
1. Select the **Create Notification** button.
1. Enter or select:
   * **Name**: A unique name for this notification.
   * **Webhook Type**: The product or proccess receiving the notification. Options: **Slack**, **Webhook**, and **ServiceNow**.
   * **Failure Type**: The Chef process for the notification. Options: **Infra Client Run failures** and ** Inspec compliance scan failures**
   * **Webhook URL**: The API address that the notification uses.
   * **ServiceNow Username**: The name you use to sign in to ServiceNow.
   * **ServiceNow Password**: The password you use to sign in to ServiceNow.
1. Select **Test Connectivity**. A successful test displays **Notification test connected successfully**. An unsuccessful test displays a message with information about any connection or credential problems to help you fix the error.

   ![Chef Automate Notification for CCR Failures](/images/automate/SNOW_CCR_Setup.png)

1. Select **Create Notification** to save the configuration details for this notification.

### Chef InSpec Scan Incidents

Follow these steps to report failed Chef InSpec scans from Chef Automate:

1. Navigate to **Settings** > **Notifications**
1. Select the **Create Notification** button.
1. Enter or select:
   * **Name**: A unique name for this notification.
   * **Webhook Type**: The product or proccess receiving the notification. Options: **Slack**, **Webhook**, and **ServiceNow**.
   * **Failure Type**: The Chef process for the notification. Options: **Infra Client Run failures** and **Inspec compliance scan failures**
   * **Webhook URL**: The API address that the notification uses.
   * **ServiceNow Username**: The name you use to sign in to ServiceNow.
   * **ServiceNow Password**: The password you use to sign in to ServiceNow.
1. Select **Test Connectivity**. A successful test displays **Notification test connected successfully**. An unsuccessful test displays a message with information about any connection or credential problems to help you fix the error.

   ![Chef Automate Notification for InSpec Scan Failures](/images/automate/SNOW_Scan_Setup.png)

1. Select **Create Notification**. The app saves the configuration for the Chef Automate InSpec scans notifications.
