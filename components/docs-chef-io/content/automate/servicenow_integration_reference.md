+++
title = "Chef Automate Integration App Reference"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Automate Integration App Reference"
    parent = "automate/integrations/servicenow"
    identifier = "automate/integrations/servicenow/servicenow_integration_reference.md ServiceNow Integration Reference"
    weight = 30
+++

This page details information on the topics that will aid you while installing and configuring the Chef Automate Integration App in the  ServiceNow ecosystem.

Refer to the [ServiceNow Integration]({{< relref "servicenow_integration" >}}) page to install and configure the Chef Automate Integration App in the ServiceNow ecosystem.

{{< note >}}

You can list the Chef Automate features in the ServiceNow instance by entering **Chef** in the **Filer Navigator** text box. All Chef features displays within **Chef Automate** menu.

{{< /note >}}

## Events

Navigate to the **Events** section by selecting **Chef Automate** > **Events** from ServiceNow.

`x_chef_automate.chef.process.data`
: Triggers when ServiceNow receives node data from either the terminal or the app.

  {{< figure src="/images/automate/snow_integration_event_registration.png" alt="Event Registration - Process Data">}}

`x_chef_automate.chef.process.installation`
: Triggers when the asset import table receives software installation details to map in the _CMDB server_ table.

  {{< figure src="/images/automate/snow_integration_event_registration1.png" alt="Event Registration - Process Installation">}}

## Scripts

### Script Actions

Navigate to the **Script Actions** section by selecting **Chef Automate** > **Script Actions** from ServiceNow.

- The **Asset Process** script action processes events and updates the node data into the _asset import_ table.

{{< figure src="/images/automate/snow_integration_script_action.png" alt="Asset Process">}}

- The **Process Software Instance** script action processes events and updates the software installation data in the _CMDB_ sever table.

{{< figure src="/images/automate/snow_integration_script_action1.png" alt="Process Software Instance">}}

### Script Includes

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

### Scripted REST API

The **Scripted Rest API** feature aids in establishing a connection between ServiceNow and the Chef Automate application with authentication. You can navigate to this section by selecting **Chef Automate** > **Script Rest API** from ServiceNow.

{{< figure src="/images/automate/snow_integration_scripted_restapi_1.png" alt="Scripted REST Service">}}

{{< figure src="/images/automate/snow_integration_scripted_restapi_2.png" alt="Scripted REST Resource">}}

## Transform Maps

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

### Transform Map Scripts

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

## Discovery

[ServiceNow's](https://www.servicenow.com/) Discovery feature finds applications and devices on your network, and then updates the Configuration Management Database (CMDB) with the information it finds. It discovers both physical and logical components, including virtual machines, servers, storage, databases, applications, and more.

Refer to ServiceNow's [Discovery](https://docs.servicenow.com/bundle/paris-it-operations-management/page/product/discovery/reference/r-discovery.html) page for information on Discovery and its types.

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

## Roles

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
