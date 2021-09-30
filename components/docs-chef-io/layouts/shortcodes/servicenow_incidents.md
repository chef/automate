### Chef Infra Client Incidents

To set up automatic incident reporting for failed Chef Infra Client runs from Chef Automate:

1. Confirm that you have the `x_chef_automate.api` role.
1. Navigate to **Settings** > **Notifications**
1. Select **Create Notification**
1. Enter or select:
   * **Name**: A unique name for this notification.
   * **Webhook Type**: The product or proccess receiving the notification. Options: **Slack**, **Webhook**, and **ServiceNow**.
   * **Failure Type**: The Chef process for the notification. Select **Infra Client Run failures**.
   * **Webhook URL**: The API address that the notification uses. It has the format: `https://ven12345.service-now.com/api/x_chef_automate/client_run`.
   * **ServiceNow Username**: The name you use to sign in to ServiceNow.
   * **ServiceNow Password**: The password you use to sign in to ServiceNow.
1. Select **Test Connectivity**. A successful test displays **Notification test connected successfully**. An unsuccessful test displays a message with information about any connection or credential problems to help you fix the error.

   ![Chef Automate Notification for CCR Failures](/images/automate/SNOW_CCR_Setup.png)

1. Select **Create Notification** to save this notification.

### Chef InSpec Scan Incidents

Follow these steps to report failed Chef InSpec scans from Chef Automate:

1. Navigate to **Settings** > **Notifications** > **Create Notification** button.
1. Enter or select:
   * **Name**: A unique name for this notification.
   * **Webhook Type**: The product or proccess receiving the notification. Select the **ServiceNow** option.
   * **Failure Type**: The Chef process for the notification. Select the **Inspec compliance scan failures** option.
   * **Webhook URL**: The API address that the notification uses. It has the format:`https://ven12345.service-now.com/api/x_chef_automate/inspec_scan`
   * **ServiceNow Username**: The name you use to sign in to ServiceNow.
   * **ServiceNow Password**: The password you use to sign in to ServiceNow.
1. Select **Test Connectivity**. A successful test displays **Notification test connected successfully**. An unsuccessful test displays a message with information about any connection or credential problems to help you fix the error.

   ![Chef Automate Notification for InSpec Scan Failures](/images/automate/SNOW_Scan_Setup.png)

1. Select **Create Notification** to save this notification.
