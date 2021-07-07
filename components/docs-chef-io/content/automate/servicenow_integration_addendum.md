+++
title = "ServiceNow Integration Components"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "ServiceNow Integration Components"
    parent = "automate/reference"
    identifier = "automate/reference/servicenow_integration_addendum.md ServiceNow Integration Components"
    weight = 60
+++

## ServiceNow Integration Components

This page includes information about the various ServiceNow components used while installing and configuring Chef Automate with CMDB App in ServiceNow.

{{< note >}}

You can list the Chef Automate features in the ServiceNow instance by typing **Chef** in the **Filer Navigator** text box. All Chef features are displayed within **Chef Automate** menu.

{{< /note >}}

### Events

You can navigate to the **Events** section by selecting **Chef Automate** >**Events** from ServiceNow.

- The `x_chef_automate.chef.process.data` event triggers whenever ServiceNow receives the node data from either terminal or through app.

{{< figure src="/images/automate/snow_integration_event_registration.png" alt="Event Registration - Process Data">}}

- The `x_chef_automate.chef.process.installation` event triggers whenever the asset import table receives software installation details to map in the _CMDB server_ table.

{{< figure src="/images/automate/snow_integration_event_registration1.png" alt="Event Registration - Process Installation">}}

### Script Actions***

You can navigate to the **Script Actions** section by selecting **Chef Automate** >**Script Actions** from ServiceNow, and make the required changes.

- The **Asset Process** script action processes the event and updates the node data into the _asset import_ table.

{{< figure src="/images/automate/snow_integration_script_action.png" alt="Asset Process">}}

- The **Process Software Instance** script action processes the event and updates the software installation data in the _CMDB_ sever table.

{{< figure src="/images/automate/snow_integration_script_action1.png" alt="Process Software Instance">}}

### Script Includes

You can navigate to the **Script Includes** section by selecting **Chef Automate** >**Script Includes** from ServiceNow, and make the required changes.

- **AutomateApi:** Calls the Chef Automate API and checks the status of the response.
- **AutomateApiClient:** Updates the token and password in the Chef Automate instance.
- **BufferToImportUtil:** Updates the CPU speed in Linux server, CMDB server, CMDB OSX server, and CMDB win server.
- **ImportUtil:** Updates the compliance import data.
- **JsonUtil:** Updates the JSON objects.
- **Logger:** Updates logging status as **enabled** or **disabled**.
- **PropertiesUtil:** Updates the role in the property.
- **Util:** Updates manufacturer data in the core company table and updates the category and manufacturer data in the _CMDB model_ table.

{{< figure src="/images/automate/snow_integration_transform_map_scripts.png" alt="Script Includes">}}

### Transform Maps

The **Transform Maps** feature maps the source table, asset import to the target table, and the CMDB Server. You can navigate to the **Transform Maps** section by selecting **Chef Automate** >**Transform Maps** from ServiceNow, and make the required changes.

{{< figure src="/images/automate/snow_integration_field_map.png" alt="Table Transaction Map">}}

#### Field Maps

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

The **Transformation** events occur while transforming an import set table into a target table.

- **On Before:** Mapping manufacturer, CPU manufacturer, model category, and model by using this script.

- **On After:** The  five **On After** scripts required to update the various values are as follows:

    1. Update **node id**, **node name**, **organization**, **IP address** and insert the record into Chef Automate client run cookbooks, Chef Automate client run recipes, Chef Automate client run lists.
    2. Update **software**, **name** and installed in _CMDB software instance_ table.
    3. Update data in the _cmdb\_ci\_spkg_ table and the _cmdb\_software\_instance_ table.
    4. Insert or update data in the _cmdb\_ci\_file\_system_ table.
    5. Insert or update data in the _cmdb\_ci\_network\_adapter_ table.

{{< figure src="/images/automate/snow_integration_transform_map_scripts1.png" alt="Transform Map Scripts">}}

### Scripted Rest API

The **Scripted Rest API** feature aids in establishing a connection between ServiceNow and the Chef Automate application with authentication. You can navigate this section by selecting **Chef Automate** >**Script Rest API** from ServiceNow.

{{< figure src="/images/automate/snow_integration_scripted_restapi_1.png" alt="Scripted REST Service">}}

{{< figure src="/images/automate/snow_integration_scripted_restapi_2.png" alt="Scripted REST Resource">}}

### Roles

Refer to the following pages on how to create users and assign roles in ServiceNow:

- [Creating Users in ServiceNow](https://docs.servicenow.com/csh?topicname=t_CreateAUser.html)
- [Assigning Roles in ServiceNow](https://docs.servicenow.com/csh?topicname=t_AssignARoleToAUser.html)

#### Role x_chef_automate.admin

The `x_chef_automate.admin` role is assigned to a user other than a System Administrator to allow administration of the application properties and logs. Thus, a user who is not a system administrator performs the administration.

Note that a System Administrator can perform all tasks that this role can. In addition, the **Admin** role is added by default in ServiceNow.

The **Admin** role grants user access to the:

- Asset Imports module
- Properties
- Automate instances
- Servers
- Chef infra client runs
- Compliance reports profiles
- Compliance report results
- Compliance Profiles
- Compliance Profiles results
- Support
- Events
- Script includes
- Transform maps
- Chef servers

#### Role x_chef_automate.user

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

Ensure that you need to assign the _OOB ITIL_ role for integration with CMDB data in ServiceNow.

#### Role x_chef_automate.api

The `x_chef_automate.api` role is suitable for users responsible for integrating the Chef Automate data into the application. It is advisable to create a new user specifically for this role. This user's credentials are needed to configure the Chef Automate server for communication with the application.

Note that for integration with CMDB data, you need to assign the _OOB ITIL_ role. In addition, you must select **Web service** access for this user role.
