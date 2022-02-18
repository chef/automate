+++
title = "ServiceNow Administrator Reference"

draft = false

gh_repo = "automate"
aliases = "servicenow_integration_reference"
[menu]
  [menu.automate]
    title = "Administrator Reference"
    parent = "automate/integrations/servicenow"
    identifier = "automate/integrations/servicenow/servicenow_integration_reference.md ServiceNow Administrator Reference"
    weight = 30
+++

This page details information on the topics that will aid you while installing and configuring the Chef Automate applications in the ServiceNow ecosystem.

- [Integration App]({{< relref "servicenow_integration" >}})
- [Incident App]({{< relref "servicenow_incident_creation" >}})

Find the Chef Automate features in the ServiceNow instance by searching **Chef** in the **Filer Navigator** . All Chef features display within **Chef Automate** menu.

## Integration App

### Benchmarking

The Chef Automate Installation team has tested the integration of the Integration App with 10K nodes of Chef Infra Client Run data. Infrastructure with more than 10K nodes may have performance issues. The system performance will decrease with other applications running in your environment.

The typical production instance of ServiceNow can have between 8-12K nodes, and will perform at +10K nodes per hour processed with Chef Infra Client run data. Benchmarking indicates that the increase in the number of nodes in the ServiceNow instance reduces the processing time.

| Client Nodes  | Total Time Taken (Dev 1 Node)  | Total Time Taken (Vendor 2 Node)  |
|---------------|--------------------------------|-----------------------------------|
| 200           | 7m48s                          | 4m4s                              |
| 500           | 17m47s                         | 10m16s                            |
| 2000          | 1h15m                          | 45m                               |
| 10,000        | 5h40m                          | 3h3m                              |

### Events

Navigate to the **Events** section by selecting **Chef Automate** > **Events** from ServiceNow.

`x_chef_automate.chef.process.data`
: Triggers when ServiceNow receives node data from either the terminal or the app.

  {{< figure src="/images/automate/snow_integration_event_registration.png" alt="Event Registration - Process Data">}}

`x_chef_automate.chef.process.installation`
: Triggers when the asset import table receives software installation details to map in the _CMDB server_ table.

  {{< figure src="/images/automate/snow_integration_event_registration1.png" alt="Event Registration - Process Installation">}}

### Scripts

#### Script Actions

Navigate to the **Script Actions** section by selecting **Chef Automate** > **Script Actions** from ServiceNow.

- The **Asset Process** script action processes events and updates the node data into the _asset import_ table.

{{< figure src="/images/automate/snow_integration_script_action.png" alt="Asset Process">}}

- The **Process Software Instance** script action processes events and updates the software installation data in the _CMDB_ sever table.

{{< figure src="/images/automate/snow_integration_script_action1.png" alt="Process Software Instance">}}

#### Script Includes

You can navigate to the **Script Includes** section by selecting **Chef Automate** > **Script Includes** from ServiceNow.

`AutomateApi`
: Calls the Chef Automate API and checks the status of the response.

`AutomateApiClient`
: Updates the token and password in the Chef Automate instance.

`BufferToImportUtil`
: Updates the CPU speed in Linux server, CMDB server, CMDB OSX server, and CMDB win server.

`ImportUtil`
: Updates the compliance import data.

`JsonUtil`
: Updates the JSON objects.

`Logger`
: Updates logging status as `enabled` or `disabled`.

`PropertiesUtil`
: Updates the role in the property.

`Util`
: Updates manufacturer data in the core company table and updates the category and manufacturer data in the _CMDB model_ table.

{{< figure src="/images/automate/snow_integration_transform_map_scripts.png" alt="Script Includes">}}

#### Scripted REST API

The **Scripted Rest API** feature aids in establishing a connection between ServiceNow and the Chef Automate application with authentication. You can navigate to this section by selecting **Chef Automate** > **Script Rest API** from ServiceNow.

{{< figure src="/images/automate/snow_integration_scripted_restapi_1.png" alt="Scripted REST Service">}}

{{< figure src="/images/automate/snow_integration_scripted_restapi_2.png" alt="Scripted REST Resource">}}

### Transform Maps

The **Transform Maps** feature maps the source table, asset import to the target table, and the CMDB Server. You can navigate to the **Transform Maps** section by selecting **Chef Automate** > **Transform Maps** from ServiceNow, and make the required changes.

{{< figure src="/images/automate/snow_integration_field_map.png" alt="Table Transaction Map">}}

### Field Maps

The **Field Maps** establishes a relationship between a field in an import set table and the target table.

| Source field | Target field |
| :---        | :---   |
| asset_tag | asset_tag |
| attributes | attributes |
| cpu_core_count | cpu_core_count |
| cpu_count | cpu_count |
| cpu_manufacturer | cpu_manufacturer |
| cpu_speed | cpu_speed |
| cpu_type | cpu_type |
| disk_space | disk_space |
| dns_domain | dns_domain |
| host_name | host_name |
| ip_address | ip_address |
| mac_address | mac_address |
| manufacturer | manufacturer |
| model | model_id |
| model_category_name | x_chef_automate_model_categoryname |
| name | name |
| os | os |
| os_service_pack | os_service_pack |
| os_version | os_version |
| ram | ram |
| serial_number | serial_number |
| short_description | short_description |
| system_class_name | sys_class_name |
| virtual | virtual |
| [Script] | install_status |

#### Transform Map Scripts

**Transformation** events occur while transforming an import set table into a target table.

`On Before`
: Mapping manufacturer, CPU manufacturer, model category, and model by using this script.

`On After`
: The five `On After` scripts required to update the values are:

  1. Update **node id**, **node name**, **organization**, **IP address** and insert the record into Chef Automate client run cookbooks, Chef Automate client run recipes, Chef Automate client run lists.
  1. Update **software**, **name** and installed in _CMDB software instance_ table.
  1. Update data in the _cmdb\_ci\_spkg_ table and the _cmdb\_software\_instance_ table.
  1. Insert or update data in the _cmdb\_ci\_file\_system_ table.
  1. Insert or update data in the _cmdb\_ci\_network\_adapter_ table.

{{< figure src="/images/automate/snow_integration_transform_map_scripts1.png" alt="Transform Map Scripts">}}

### Identification and Reconciliation

The Identification and Reconciliation rules maintain the integrity of the CMDB by managing duplicate CIs and controlling updates to CIs when multiple data sources are used to create and update CI records. These rules help prevent duplication of CI records, reconcile CI attributes, reclassify CIs, and allow authoritative data sources to update CI records in the CMDB.

Identification rules identify new CIs and existing CIs. Identification rules apply to a CI class and consist of a single CI identifier with one or more entries that match CIs based on related attributes or by tables of related CIs.

Reconciliation is the synchronization of two or more matching database segments to ensure consistency across them. Reconciliation rules specify which discovery sources can update a table or a set of table attributes, and the precedence order among these discovery sources. You can define these rules at the parent or at the child class level. Without reconciliation rules, discovery sources may be overwritten by updates to attribute from other discovery sources.

Refer to ServiceNow's [CMDB Identification and Reconciliation](https://docs.servicenow.com/bundle/quebec-servicenow-platform/page/product/configuration-management/concept/c_CMDBIdentifyandReconcile.html) page for detailed information on these rules.

You can view the reconciliation rules by selecting **CI Class Manager** > **Hierarchy** > **CI Classes** > **Reconciliation Rules** from ServiceNow.

{{< figure src="/images/automate/snow_integration_reconcile.png" alt="Reconciliation Rules">}}

Refer to ServiceNow's [Create a CI reconciliation rule](https://docs.servicenow.com/bundle/quebec-servicenow-platform/page/product/configuration-management/task/t_CreateCIReconciliationRule.html) page on how to set a reconciliation rule.

{{< figure src="/images/automate/snow_integration_reconcile1.png" alt="Create Reconcile Rule">}}

{{< note >}}

The Chef Automate Integration App does not provide any reconciliation rules.

{{< /note >}}

### Discovery

[ServiceNow's](https://www.servicenow.com/) Discovery feature finds applications and devices on your network, and then updates the Configuration Management Database (CMDB) with the information it finds. It discovers both physical and logical components, including virtual machines, servers, storage, databases, applications, and more.

Refer to ServiceNow's [Discovery](https://docs.servicenow.com/bundle/paris-it-operations-management/page/product/discovery/reference/r-discovery.html) page for information on Discovery and its types.

## Integration App Roles

You can associate a single user with more than one roles.

### Role x_chef_automate.admin

You can assign the `x_chef_automate.admin` role to a user other than a System Administrator to allow another user to manage the application properties and logs.

The System Administrator authorization includes access to the tasks in the **Admin** role.

The **Admin** role grants user access to the:

- Asset Imports module
- Properties
- Chef Automate instances
- Servers
- Chef Infra Client runs
- Compliance reports profiles
- Compliance report results
- Compliance Profiles
- Compliance Profiles results
- Support
- Events
- Script includes
- Transform maps
- Chef Infra Servers

### Role x_chef_automate.user

The `x_chef_automate.user` role is suitable for those users who require application access without administration rights. The role grants a user access to the:

- Chef Automate menu
- Servers module
- Client runs module
- CI cookbooks module
- Compliance reports module
- Compliance reports profiles module
- Compliance reports module
- Compliance profiles module
- Compliance profiles controls module
- Logs module
- Properties module
- Support module

{{< note >}}
For integration with CMDB data, you need to assign the _OOB ITIL_ role.role for integration with CMDB data in ServiceNow.
{{< /note >}}

### Role x_chef_automate.api

The `x_chef_automate.api` role is suitable for users responsible for integrating the Chef Automate data into ServiceNow. We recommend creating a new user specifically for this role. The Chef Automate Integration App requires the API role to set up communication with Chef Automate.

{{< note >}}
For integration with CMDB data, you need to assign the _OOB ITIL_ role. Also, you must select **Web service** access for this user role.
{{< /note >}}

## Integration App Properties

### Chef Automate Properties

`feed_interval`
: The frequency in hours for refreshing the data feed. The duration between data feed refreshes is proportional to the node count, with more nodes requiring higher settings. Valid values: Any integer in the range of `2` to `8`. Default: `4`.

`node_batch_size`
: The testing executed for a benchmark. The size of a compliance scan is proportional to the number of profiles applied. Scans exceeding 4MB may fail or display incorrectly in reports. Use a lower `node_batch_size` setting to reduce the number of profiles applied in a single batch. Valid values: Between `1` and `30`.  Default: `15`.

### Application Properties

The Integration App has nine configurable **Application Properties**. Changing these settings requires the ServiceNow `admin` or `x_chef_automate.admin` permissions.

`x_chef_automate.chef.default.status`
: Used to set up the status of the service record as **inserted** or **updated**. Default: `1`.

`x_chef_automate.client_runs_per_node`
: Used to set a highest number of clients runs for a node. Default: `5`.

`x_chef_automate.compliance_reports_per_node`
: Used to set a highest number of compliance reports for a node. Default: `5`.

`x_chef_automate.insert_manufacturer`
: Inserts the new record during the import if a model is not found in the _core\_company_ table by settingthe property to **Yes**. Default: `Yes`.

`x_chef_automate.insert_model`
: Inserts the new record during the import if a model is not found in the _cmdb\_model_ table by setting the property to **Yes**. Default: `Yes`

`x_chef_automate. logging.enabled`
: Used to flag the logging with **enable** or **disable** values. Default: `No`.

`x_chef_automate.logging.verbosity`
: Debugs the data in ServiceNow. Enables the selected logging level and is visible in logs. Valid values: `Debug`, `Warn`, `Info`, `Error`. Default: `Error`.

`x_chef_automate.Rest.api`
: Enables the Chef Automate API from ServiceNow. Set to `Yes` to enable and `No` to disable. Valid Values: `Yes`, `No`. Default: `Yes`.

`x_chef_automate.enable.system.app`
: Used to enable software installed mappings. Valid Values: `Yes`, `No`. Default: `No`.

## Incident App Roles

You can associate a single user with more than one role.

### Role `x_chef_incident.admin`

You can assign the `x_chef_incident.admin` role to a user other than a System Administrator to allow another user to manage the application properties and logs.

The System Administrator authorization includes access to the tasks in the **Admin** role.

The **Admin** role grants user access to the:

- Chef incidents menu
- Client runs menu item
- Chef InSpec scans menu item
- Properties menu item
- Support menu item
- Logs menu item

### Role `x_chef_incident.user`

The `x_chef_incident.user` role is suitable for those users who require application access without administration rights. The role grants a user access to the:

- Chef Incidents menu
- Chef Infra Client runs menu item
- Chef InSpec scans menu item
- Support menu item

{{< note >}}

Client run and Chef InSpec scan records are linked to incidents and is appropriate to assign this role to users who manage incidents in ServiceNow.

{{< /note >}}

### Role x_chef_incident.api

The `x_chef_incident.api` role is suitable for users responsible for integrating the Chef Automate data into ServiceNow. We recommend creating a new user specifically for this role. The Chef Automate Incident App requires the API role to set up communication with Chef Automate.

## Incident App Properties

`x_chef_incident.association`
: Used to associate a Chef Infra Client run record with an Incident record. The possible values are: `cookbook` and `node`. Default: `cookbook`.

Setting the value to `cookbook` creates an incident for the failed cookbook. All failing Chef Infra Client runs on nodes associates with the corresponding incident. `cookbook` is the default value as the number of nodes exceeds the number of cookbooks. The short description of the incident indicates the failed cookbook:

![CCR Failed Cookbook Description](/images/automate/SNOW_Failed_Cookbook.png)

The **Chef Infra Client runs** tab of the incident displays the associated client runs. Setting the value to `node` creates an incident for each failed node. All failing Chef Infra Client runs for a node associates with the corresponding incident. The short description of the incident indicates the failed node.

![CCR Failed Node Description](/images/automate/SNOW_Failed_Node_CCR.png)

`x_chef_incident.scan_association`

: Used to associate the Chef InSpec scan record with an incident record. The possible values are: `profile` and `node`. Deault: `profile`.

Setting the value to `profile` creates an incident for the failed Chef InSpec compliance profile. All Chef InSpec scans on failing nodes associates with the corresponding incident. `profile` is the default value as the number of nodes exceeds the number of profiles. The short description of the incident indicates the failed profile.

![Scan Failed Profile Description](/images/automate/SNOW_Failed_Profile_Scan.png)

The **Chef InSpec scans** tab of the incident displays the associated Chef InSpec scans. Setting the value to `node` creates an incident for each failed node. All Chef InSpec scans failing for a node associates with the corresponding incident. The short description of the incident indicates the failed node.

![Scan Failed Node Description](/images/automate/SNOW_Failed_Node_Scan.png)

`x_chef_incident.assigned_to`

: Used to set up the ServiceNow user ID to assign incidents raised by the application. The corresponding group assigns automatically to the incident if the user is in an assignment group. Default: `blank`.

`x_chef_incident.assignment_group`

: Used to set up a group rather than an individual user in the `x_chef_incident.assigned_to` property to assign the incident to a group. Default: `blank`.

`x_chef_incident.impact`

: Used to set an impact value in an incident raised by the application. Possible values are `1`, `2` or `3`. Default: `2`.

`x_chef_incident.urgency`

: Used to set an urgency value in an incident raised by the application. Possible values are `1`, `2` or `3`. Default: `2`.

`x_chef_incident.retention_days`

: Used to define number of days to maintain the Chef Infra Client run and Chef InSpec scan records in ServiceNow. Default: `30`.

The ServiceNow app deletes these records of the corresponding closed incidents, deleted incidents, and the removed incidents from the Chef Infra Client run or Chef InSpec scan record by a user update.

`x_chef_incident.logging.enabled`

: Used to flag the logging with **enable** or **disable** status. Once enabled, you can view the logs from the **Chef incidents** > **Logs** menu and the **System logs** > **Application logs** menu by authorized users. Default: `No`.

`x_chef_incident.logging.verbosity`

: Used to debug the data in ServiceNow. Enables the selected logging level and is visible in logs. The possible values are `debug`, `warn`, `info`, and `error`. Default: `error`.

<!-- ## Event Creation App Roles

You can associate a single user with more than one role.

### Role `x_chef_event.admin`

You can assign the `x_chef_event.admin` role to a user other than a System Administrator to manage the application properties and logs.

The System Administrator authorization includes access to the tasks in the **Admin** role.

The **Admin** role grants user access to the following:

- Chef Events menu
- Client Infra Client Run Alerts menu item
- Chef InSpec scans Alerts menu item
- Client Infra Client Runs
- Chef InSpec scans
- Properties menu item
- Support menu item
- Logs menu item

### Role `x_chef_event.chef_inspec_scans_user`

The `x_chef_event.chef_inspec_scans_user` role provides a user access to applications. It does not provide administration privileges. Client run and Chef InSpec scan records are linked to events, and is appropriate to assign this role to users who manage events in ServiceNow.

This role grants a user access to the:

- Chef Events menu
- Chef Infra scans Alerts menu item
- Chef InSpec scans menu item
- Support menu item

### Role x_chef_event.api

The `x_chef_event.api` role is:

- Required to set up the Event Creation App with Chef Automate.
- Necessary for users integrating Chef Automate and ServiceNow.
- The best practice is to create a user for this role.

suitable for users responsible for integrating the Chef Automate data into ServiceNow. We recommend creating a new user specifically for this role. The Chef Automate Event Creation App requires the API role to set up communication with Chef Automate.

### Event Creation App Properties

`x_chef_event.client_run_message_key`
: Used to associate a Chef Infra client run record with an event record. Possible values: `cookbook` and `node`. Default: `cookbook`.

Set the value to `cookbook` to create an event for the failed cookbook. This associates all failing Chef Infra client runs with this cookbook failure with the event. `cookbook` is the default value because the number of cookbooks is independent of the number of nodes in any system. The message key description of the event provides information about the failure.

TODO: [Chef Events Client Runs page] data not available

The **Chef Infra Client Run Alerts** tab of the alerts displays the associated client run events. Setting the value to `node` creates an event for each failed node. All failing Chef Infra client runs for a node associated with the corresponding event. The message key description of the event provides information about the run failure for a node.

![Chef Events Client Run Alerts page](/images/automate/sn_event_clientrun_alerts.png)

`x_chef_event.inspec_scan_message_key`

: Used to associate a Chef InSpec scan record with an event record. Possible values: `profile` and `node`. Default: `profile`.

Set the value to `profile` to create an event for the failed chef InSpec compliance profile with a message value. This associate all failing Chef InSpec compliance scans with the corresponding alerts. `profile` is the default value because the number of cookbooks is independent of the number of nodes in any system. The message key description of the event provides information about the failure.

![Chef Events InSpec Scans page](/images/automate/sn_event_inspecscans.png)

The **Chef InSpec Scan Alerts** tab of the event displays the associated Chef InSpec scans. Setting the value to `node` creates an event for each failed node. All failing Chef InSpec scans for a node associated with the corresponding event. The message key description of the event indicates the failed node.

![Chef Events InSpec Scan Alerts page](/images/automate/sn_event_inspecscans_alerts.png)

`x_chef_events.logging.enabled`

: Set to `Yes` to enable logging and `No` to disable it. Once enabled, authorized users can view the logs at **Chef Events** > **Logs** and **System logs** > **Application logs**. Default: `No`.

![Chef Events Properties page](/images/automate/sn_event_app_properties.png) -->
