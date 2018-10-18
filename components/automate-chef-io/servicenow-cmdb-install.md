+++
title = "ServiceNow CMDB Sync Integration"
description = "Installing and Configuring the Automate ServiceNow Integration for CMDB Sync"
draft = false 
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "reference"
    weight = 30
+++

## Overview

This guide helps you set up a ServiceNow instance that syncs asset data from Chef Automate runs to your ServiceNow CMDB.

### Chef Automate - ServiceNow CMDB Sync Integration

The integration between a Chef Automate server and a ServiceNow instance requires the following:

* Chef Automate
* Chef CMDB Sync Application in ServiceNow

The Chef CMDB Sync application is a ServiceNow certified scoped application available from the ServiceNow store. [Chef Automate](https://www.chef.io/automate/) provides a full suite of enterprise capabilities for workflow, node visibility, and compliance. Once configured, the Chef Automate server sends HTTPS JSON notifications to the Chef CMDB Sync Application in a ServiceNow instance to populate the CMDB.

![ServiceNow and Automate Flow](/images/docs/SNOW_Automate_diagram.png)

#### Prerequisites

* The ServiceNow instance must be publicly reachable on https port 443
* [Chef automate server installation](https://docs.chef.io/chef_automate.html)
* ServiceNow package - System Import Sets com.glide.system_import_set, min version 1.0.0

## Configuration

### Install Chef CMDB Sync Application in ServiceNow

The Chef CMDB Sync application exposes the REST API endpoints that facilitate communication between Chef Automate and the ServiceNow instance.

* Visit the ServiceNow store at [https://store.servicenow.com](https://store.servicenow.com)
* Get the Chef CMDB Sync application
* In the ServiceNow instance, navigate to the System Applications > Applications menu
* From the Downloads tab, install the Chef Automate CMDB Sync application

### Create Application Users

The application provides several roles appropriate for integration, which can be assigned to existing or new ServiceNow users. The roles are as follows:

* `x_chef_cmdb.admin`
* `x_chef_cmdb.user`
* `x_chef_cmdb.api`

For more information, see [Creating users in ServiceNow](https://docs.servicenow.com/bundle/kingston-platform-administration/page/administer/users-and-groups/task/t_CreateAUser.html) and [Assigning roles in ServiceNow](https://docs.servicenow.com/bundle/kingston-platform-administration/page/administer/users-and-groups/task/t_AssignARoleToAUser.html)

#### Role x_chef_cmdb.admin

This `x_chef_cmdb.admin` role can be assigned to a user other than a systems administrator to allow administration of the application properties and logs. Thus, administration can be carried out by a user who is not the system administrator. Note that a systems administrator can perform all tasks that a `x_chef_cmdb.admin` role can.

The admin role grants a user access to the:

* Chef CMDB menu
* Asset Imports menu item
* Cookbook Nodes menu item
* Cookbooks menu item
* Properties menu item
* Support menu item
* Logs menu item

#### Role x_chef_cmdb.user

The role is suitable for users that require application access without administration access. The role grants a user access to the:

* Chef CMDB menu
* Asset Imports menu item
* Cookbook Nodes menu item
* Cookbooks menu item
* Support menu item
* Logs menu item

#### Role x_chef_cmdb.api

The `x_chef_cmdb.api` role should be assigned to a user that is responsible for integrating the Chef Automate data into the application. Create a new user specifically for this task. Communication with the application requires this user's credentials.

#### Application Properties

The application properties can be configured by users with admin or `x_chef_cmdb.admin` roles. Select the Chef CMDB > Properties menu item to navigate to the properties configuration UI.

![ServiceNow Config Page](/images/docs/SNOW_cmdb_properties_page.png)

The application system properties are:

* `x_chef_cmdb.company`
* `x_chef_cmdb.hostname`
* `x_chef_cmdb.ipAddress`
* `x_chef_cmdb.macAddress`
* `x_chef_cmdb.cookbook_cleanup`
* `x_chef_cmdb.logging.enabled`

#### Property x_chef_cmdb.company

The `x_chef_cmdb.company` property defines how the default company name that will be asscociated with Chef synced configuration items.

#### Property x_chef_cmdb.hostname

The `x_chef_cmdb.hostname` property informs the app whether use node's hostname in the querty to distinguish between a new and exiting configuration item.

#### Property x_chef_cmdb.ipAddress

The `x_chef_cmdb.ipAddress property` property informs the app whether use node's ip address in the querty to distinguish between a new and exiting configuration item.

#### Property x_chef_cmdb.macAddress

The `x_chef_cmdb.macAddress` property informs the app whether use node's mac address in the querty to distinguish between a new and exiting configuration item.

#### Property x_chef_cmdb.cookbook_cleanup

The `x_chef_cmdb.cookbook_cleanup` property is the number of days to retain stale cookbook records in the Cookbooks table. The default is set to 30 days. A stale cookbook is one which has not been updated in the specified number in days since the current date.

#### Property x_chef_cmdb.logging.enabled

The `x_chef_cmdb.logging.enabled` property allows application logging to be enabled. Once enabled, logs can be viewed from the Chef CMDB > Logs menu and the System logs > Application logs menu by authorized users. The default value is `false` and logs are disabled by default.

### Configure Chef Automate

Chef Automate can be configured to send requests to the REST API exposed by the application:

* Chef CMDB Asset Sync API /api/x_chef_cmdb/v1/asset

#### Chef CMDB Sync API

The Chef Automate event feed.

Todo: Waiting for A2

# Uninstalling

To uninstall the application:

* In the ServiceNow instance, navigate to the System Applications > Applications menu
* From the Downloads tab, click on the Chef Automate CMDB Sync link
* In the Related Links section, click uninstall