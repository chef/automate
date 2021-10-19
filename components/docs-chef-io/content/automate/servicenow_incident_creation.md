+++
title = "Chef Automate Incident Creation App"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Automate Incident Creation App"
    parent = "automate/reference"
    identifier = "automate/reference/servicenow_incident_creation.md Chef Automate Incident Creation"
    weight = 40
+++

[ServiceNow](https://www.servicenow.com/) provides cloud-based services that automate enterprise IT operations. ServiceNow
specializes in IT service management (ITSM) applications and provides forms-based workflow application development. It supports
third-party applications and data integrations. The most common integrations are configuration management database (CMDB),
incident management, problem management, change management, user administration, and single sign-on authentication.

The Chef Automate Incident Creation App for ServiceNow is a certified app available in the [ServiceNow](https://store.servicenow.com) store. The Incident App generates incidents in the ServiceNow Incident Management environment for configuration run or compliance check failures in Chef Automate. This helps you to capture the failures in your automated infrastructure and improve your incident tracking and resolution.

![ServiceNow and Chef Automate Flow](/images/automate/SNOW_Automate_diagram.png)

## Key Features of Chef Automate Incident Creation App

* Incident management for infrastructure and compliance automation &mdash; Creates ServiceNow incident tickets, alert/page operations or information security (for Chef failures or InSpec failures, respectively) to provide a resolution to the automation and compliance failures at the earliest and in concordance with a customer- or regulatory-mandated SLAs or OLAs.

* Intelligent data management and event de-duplication &mdash; Promotes Chef Automate as a central data aggregator and restricts opening of a new incident tickets for the repeated failures by updating relevant fields in ServiceNow, such as the time of last failure. This helps reduce alert noise, in contrast to non-Chef Automate approaches that send data directly from managed nodes to ServiceNow.

* Compliance-related integration opportunities with other capabilities on ServiceNow &mdash; The Incident App generates a data stream of compliance events that you can leverage by using this data to other ServiceNow applications. For example, you can integrate the Chef Automate compliance scan data with Governance and Risk Compliance (GRC) or Security and Incident Management (SIEM) systems in ServiceNow. You can personalize the Chef Automate compliance data stream by prioritizing the scan results, which helps you enhance your risk dashboard with real-time and ranked compliance events instead of low-information and context-free standard data streams.

## Installation

[Chef Automate](https://docs.chef.io/automate/) provides a full suite of enterprise capabilities for workflow, node visibility, and compliance. The integration between a Chef Automate server and a ServiceNow instance requires the following components:

* Chef Automate Certified Application
* Chef Automate Server

The installation personnel performing the task of integrating the Chef Automate Incident app in the ServiceNow ecosystem must have a basic understanding of the ServiceNow system.

### System Requirements

The ServiceNow instance must be reachable on port **443** from Chef Automate.

### Software Requirements

A running [Chef Automate](https://www.chef.io/products/chef-automate) instance.
A running [ServiceNow](https://www.servicenow.com/) instance.

### Required ServiceNow Plugins

Install following ServiceNow plugins:

* System Import Sets com.glide.system_import_set, min version 1.0.0.
* Chef Automate Incident Creation 1.0.0 or higher
* [Chef Automate Server Installation](https://docs.chef.io/automate/install/).
* Chef Automate 2 Server

Locate the **System Import Sets** plugin by navigating to the **System Applications** > **All Available Applications** > **All** section in the ServiceNow application.

{{< figure src="/images/automate/snow_incident_plugin.png" alt="System Import Set Plugin">}}

### Install the Chef Automate Incident Creation App

The Chef Automate Incident Creation app exposes the REST API endpoint for communication between Chef Automate and the ServiceNow instance. Chef Automate sends HTTPS JSON notifications to the Chef Automate Incident Creation app in a ServiceNow instance to creates and update incident failures.

1. Navigate to the [ServiceNow](https://store.servicenow.com) store.
2. Search for **Chef Automate**.
3. Select the **Chef Automate Incident Creation App**.
4. Select **Get** and follow the instructions by specifying your ServiceNow credentials.
5. Open your **ServiceNow Service Management** application.
6. Select **System Applications** > **All Available Applications** > **All** menu.
7. Find the application using the filter criteria and search bar.
   You can search for the application by its `Name` or `ID`. If you cannot find an application, you may have to request it from the ServiceNow Store.
8. Select **Install**.

### Assign User Roles

The ServiceNow users must have the `x_chef_automate.api` role to set up the ServiceNow instance to receive data from Chef Automate. Changing the **Application Properties** defaults requires the `x_chef_automate.admin` role.

The Chef Automate Incident app has three default roles:

* [Role x_chef_incident.admin]({{< relref "#role_x_chef_incident.admin" >}})
* [Role x_chef_incident.user]({{< relref "#role_x_chef_incident.user" >}})
* [Role x_chef_incident.api]({{< relref "#role_x_chef_incident.api" >}})

You can assign these roles to the existing or new ServiceNow users. These roles are part of the package. If there is a need for further restrictions, you can create the required roles and controls. Also, you can associate a single user with more than one role.

The `x_chef_incident.user` role is suitable for those users who require application access without administration rights. To alter the system properties, you need to have the `x_chef_incident.admin` role.

Refer to the following pages on how to create users and assign roles in ServiceNow:

* [Creating Users in ServiceNow](https://docs.servicenow.com/csh?topicname=t_CreateAUser.html)
* [Assigning Roles in ServiceNow](https://docs.servicenow.com/csh?topicname=t_AssignARoleToAUser.html)

## Configuration

You can configure Chef Automate to send a notification to ServiceNow with REST APIs details exposed by the Chef Automate application, which are as follows:

* Chef Infra Client run API: `/api/x_chef_incident/client_run`
* Chef InSpec scan API: `/api/x_chef_incident/inspec_scan`

### Chef Infra Client Run API

Follow these steps to report the failed client runs on nodes managed by Chef Automate:

1. Confirm that you have the `x_chef_automate.api` role.
2. Navigate to **Settings** from the **Chef Automate** menu.
3. Select the **Notifications** link from **General Settings** section on the left.
4. Select the **Create Notification** button.
5. Enter the following information, which are mandatory:
   * **Name**: a unique name for the failed client run notification.
   * **Webhook Type**: ServiceNow notification type. Possible values are **Slack**, **Webhook**, and **ServiceNow**.
   * **Failure Type**: select **Chef Infra Client run failures**.
   * **Webhook URL**: ServiceNow client run API address.
   * **Username** and **Password**: a ServiceNow user credentials with `x_chef_automate.api` role. When you select **Webhook** type as **ServiceNow**, the app displays the **Username** and **Password** fields.
6. Select the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes.

   A successful test displays the **Notification test connected successfully for cl** message for client run failures. Else, an error message shows detailing the connectivity or credentials issues and helps you resolve the error.

   ![Chef Automate Notification for CCR Failures](/images/automate/SNOW_CCR_Setup.png)

7. Select **Create Notification**. The app saves the configuration details for the Chef Automate client runs notifications.

### Chef InSpec Scan API

Follow these steps to report the failed Chef InSpec scans on node managed by Chef Automate:

1. Confirm that you have the `x_chef_automate.api` role.
2. Navigate to **Settings** from the **Chef Automate** menu.
3. Select the **Notifications** link from **General Settings** section on the left.
4. Select the **Create Notification** button.
5. Enter the following information, which are mandatory:
   * **Name**: a unique name for the failed client run notification.
   * **Webhook Type**: ServiceNow notification type. Possible values are **Slack**, **Webhook**, and **ServiceNow**.
   * **Failure Type**: select **InSpec compliance scan failures**.
   * **Webhook URL**: ServiceNow Chef InSpec scan API address.
   * **Username** and **Password**: a ServiceNow user credentials with `x_chef_automate.api` role. When you select **Webhook** type as **ServiceNow**, the app displays the **Username** and **Password** fields.
6. Select the checkbox to send the compliance scan notification only when critical control failures occurs.
7. Select the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes.

   A successful test displays the **Notification test connected successfully for com** message for compliance scan failures. Else, an error message shows detailing the connectivity or credentials issues and helps you resolve the error.

   ![Chef Automate Notification for InSpec Scan Failures](/images/automate/SNOW_Scan_Setup.png)

8. Select **Create Notification**. The app saves the configuration for the Chef Automate InSpec scans notifications.

### Configure Application Properties

The Chef Automate Incident App has nine configurable **Application Properties**. You must have the `admin` or `x_chef_incident.admin` roles to change the default values in the **Application Properties**.

Follow these steps to alter the default values in the **Application Properties** screen:

1. Navigate to the **ServiceNow** application.
1. Select the **Chef Incidents** > **Properties** option from the left navigation pane. The **Chef Incident Properties** screen appears.

   {{< figure src="/images/automate/snow_incident_appproperties.png" alt="Chef Automate Incident Properties">}}

1. Make the required changes.
1. Select **Save**.

![ServiceNow Config Page](/images/automate/SNOW_config_page.png)

`x_chef_incident.association`
: Used to associate a Chef Infra Client run record with an Incident record. Possible values are: `cookbook` and `node`. Default: `cookbook`.

Setting the value to `cookbook` creates an incident for the failed cookbook. This associates all failing Chef Infra Client runs on nodes associates with the corresponding incident. `cookbook` is the default value as the number of nodes exceeds the number of cookbooks in any system. The short description of the incident provides information about the failure:

![CCR Failed Cookbook Description](/images/automate/SNOW_Failed_Cookbook.png)

The **Chef Infra Client runs** tab of the incident displays the associated client runs. Setting the value to `node` creates an incident for each failed node. All failing Chef Infra Client runs for a node associates with the corresponding incident. The short description of the incident provides information about the run failure for one node.

![CCR Failed Node Description](/images/automate/SNOW_Failed_Node_CCR.png)

`x_chef_incident.scan_association`

: Associate a Chef InSpec scan record with an incident record. Possible values are: `profile` and `node`. Deault: `profile`.

Create a Chef InSpec compliance scan incident by setting this value to `profile`. This associates all failed Chef InSpec scans with the corresponding incident. `profile` is the default value because the number of nodes exceeds the number of profiles. The short description of the incident provides information about the failure.

![Scan Failed Profile Description](/images/automate/SNOW_Failed_Profile_Scan.png)

The **Chef InSpec scans** tab of the incident displays the associated Chef InSpec scans. Setting the value to `node` creates an incident for each failed node. All Chef InSpec scans failing for a node associates with the corresponding incident. The short description of the incident indicates the failed node.

![Scan Failed Node Description](/images/automate/SNOW_Failed_Node_Scan.png)

`x_chef_incident.assigned_to`

: Assign a ServiceNow user ID to incidents. If the user is part of a group, then that group is also automatically assigned to the incident. Default: `none`.

`x_chef_incident.assignment_group`

: Assign a group to the incident instead of the individual user in the `x_chef_incident.assigned_to` property. Default: `blank`.

`x_chef_incident.impact`

: Set an incident impact value. Possible values: `1`, `2`, or `3`. Default: `2`.

`x_chef_incident.urgency`

: Set an incident urgency value. Possible values are `1`, `2`, or `3`. Default: `2`.

`x_chef_incident.retention_days`

: Define the number of days to retain Chef Infra Client run and Chef InSpec scan reports in ServiceNow. The ServiceNow app automatically updates the records associated with reports when they are closed, deleted, or removed. Default: `30`.

`x_chef_incident.logging.enabled`

: Set to `Yes` to enable logging and `No` to disable it. Once enabled, authorized users can view the logs at **Chef incidents** > **Logs** and **System logs** > **Application logs**. Default: `No`.

`x_chef_incident.logging.verbosity`

: Set the amount of information visible in logs. Possible values: `debug`, `warn`, `info`, and `error`. Default: `error`.

## Roles

You can associate a single user with more than one role.

### Role `x_chef_incident.admin`

You can assign the `x_chef_incident.admin` role to a user other than a System Administrator to allow another user to manage the application properties and logs.

The System Administrator authorization includes access to the tasks in the **Admin** role.

The **Admin** role grants user access to the:

* Chef incidents menu
* Client runs menu item
* Chef InSpec scans menu item
* Properties menu item
* Support menu item
* Logs menu item

### Role `x_chef_incident.user`

The `x_chef_incident.user` role is suitable for those users who require application access without administration rights. The role grants a user access to the:

* Chef Incidents menu
* Chef Infra Client runs menu item
* Chef InSpec scans menu item
* Support menu item

{{< note >}}

Client run and Chef InSpec scan records are linked to incidents and is appropriate to assign this role to users who manage incidents in ServiceNow.

{{< /note >}}

#### Role x_chef_incident.api

The `x_chef_incident.api` role is suitable for users responsible for integrating the Chef Automate data into ServiceNow. We recommend creating a new user specifically for this role. The Chef Automate Incident App requires the API role to set up communication with Chef Automate.

## Uninstall

To uninstall the Chef Automate Incident App:

1. In the ServiceNow instance, navigate to the **System Applications** > **Applications** menu.
1. Open the **Downloads** tab and select the **Chef Automate Incident Creation** link.
1. Navigate to the **Related Links** section.
1. select **Uninstall**.
