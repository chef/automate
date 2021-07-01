+++
title = "ServiceNow Integration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "ServiceNow Integration"
    parent = "automate/reference"
    identifier = "automate/reference/servicenow_integration.md ServiceNow Integration"
    weight = 40
+++


## Chef Automate - CMDB Integration App in ServiceNow

This guide provides instructions to configure a customer's instance for enabling the population of the Configuration Management Database (CMDB) data within ServiceNow. Chef Automate manages the CMDB data from each node in the customer's estate and the compliance and configuration management status across the Enterprise.

## Chef Automate and ServiceNow Integration

The integration between a Chef Automate server and a ServiceNow instance requires the following components:

- Chef Automate Scoped Application
- Chef Automate Server

The Chef Automate application is a ServiceNow certified scoped application available from the ServiceNow store.  It integrates with the Chef Automate infrastructure and compliance functionality.  

The [Chef Automate](https://www.chef.io/automate/) provides a full suite of enterprise capabilities for workflow, node visibility, and compliance. The Chef Automate server sends HTTPS JSON data feeds to the Chef Automate application in a ServiceNow instance to create and update the _ServiceNow CMDB_ tables, the application client run and compliance report tables. 

{{< figure src="/images/automate/snow_integration_dataflow_diagram.png" alt="Data Flow Diagram">}}

### Prerequisites

- The ServiceNow instance must be publicly reachable on https port 443. 
- [Chef Automate Server](https://docs.chef.io/chef_automate.html).
- ServiceNow package - System Import Sets `com.glide.system_import_set`, min version 1.0.0.
- ServiceNow package - Configuration Management (CMDB) 1.1.
- ServiceNow plugin - Configuration Management for Scoped Apps [com.snc.cmdb.scoped] 1.0.0.

## Installation

### Installing Chef Automate application in ServiceNow

The Chef Automate application exposes the REST API endpoint that facilitates the communication between Chef Automate and the ServiceNow instance.

- Visit the ServiceNow store at <https://store.servicenow.com>.
- Get the **Chef Automate Integration App** application.
- From the ServiceNow instance, navigate to the **System Applications** > **Applications** menu.
- From the **Downloads** tab, install the **Chef Automate** application.

### Creating Application Users

The Chef Automate application provides several roles appropriate for integration. The existing or new ServiceNow users can be assigned these roles, which are as follows:

- x_chef_automate.admin
- x_chef_automate.user
- x_chef_automate.api

These roles are part of the package. Users can create their requisite roles and controls if any further restrictions are needed.

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

Note that for integration with CMDB data, you need to assign the _OOB ITIL_ role. In addition, you must select **Web service** access only for this user role.

### References

- [Creating Users in ServiceNow] (https://docs.servicenow.com/csh?topicname=t_CreateAUser.html)
- [Assigning Roles in ServiceNow] (https://docs.servicenow.com/csh?topicname=t_AssignARoleToAUser.html)

### Configuring Application Properties

Users can configure the application properties with the `admin` or `x_chef_automate.admin` roles. 

Select the **Chef Automate** > **Properties** menu item to navigate to the **Properties** configuration UI.

{{< figure src="/images/automate/snow_integration_appproperties.png" alt="Chef Automate Properties">}}

The application system properties are:

| Property Name | Description | Default |
| --------------| ----------  | --------| 
| `x_chef_automate.chef.default.status` | Used to set up the status of the service record as **inserted** or **updated**. | Default: `1`| 
| `x_chef_automate.client_runs_per_node` | Used to set a maximum number of clients runs for a node. | Default: `5` | 
| `x_chef_automate.compliance_reports_per_node` | Used to set a maximum number of compliance reports for a node. | Default: `5` | 
| `x_chef_automate.insert_manufacturer` | Inserts the new record during the import if a model is not found in the _core\_company_ table by setting the property to **Yes**. | Default: `Yes` | 
| `x_chef_automate.insert_model` | Inserts the new record during the import if a model is not found in the _cmdb\_model_ table by setting the property to **Yes**. | Default: `Yes` | 
| `x_chef_automate. logging.enabled` | Used to flag the logging with **enable** or **disable** values. | Default: `No` | 
| `x_chef_automate.logging.verbosity` | Debugs the data in ServiceNow. The possible values are: <ul><li>Debug</li><li>Warn</li><li>Info</li> <li>Error</li></ul>It enables the selected logging level and is visible in logs. | Default: `Error` | 
| `x_chef_automate.Rest.api` | Enables the Chef Automate API from ServiceNow when Turn on REST API is set to `Yes`. The possible values are: Yes, No. | Default: `Yes` | 
| `x_chef_automate.enable.system.app` | Used to enable software installed mappings. | Default: `No` | 

### Scripted REST API

The _Scripted REST API_ is used to establishes a connection between ServiceNow and the Chef Automate application with authentication.

{{< figure src="/images/automate/snow_integration_scripted_restapi_1.png" alt="Scripted REST Service]">}}

{{< figure src="/images/automate/snow_integration_scripted_restapi_2.png" alt="Scripted REST Resource">}}

#### Event

- The `x_chef_automate.chef.process.data` event is triggered whenever ServiceNow receives the node data from either terminal or through app.

{{< figure src="/images/automate/snow_integration_event_registration.png" alt="Event Registration">}}

- The `x_chef_automate.chef.process.installation` event is triggered whenever the asset import table receives software installation details to map in the _CMDB server_ table.

{{< figure src="/images/automate/snow_integration_event_registration1.png" alt="Event Registration">}}

#### Script Action

The **Asset Process** script action processes the event and updates the node data into the _asset import_ table.

{{< figure src="/images/automate/snow_integration_script_action.png" alt="Script Action]">}}

The **Process Software Instance** script action processes the event and updates the software installation data in the _CMDB_ sever table.

{{< figure src="/images/automate/snow_integration_script_action1.png" alt="Script Action]">}}

#### Transform Map

The **Transform Map** feature maps the source table, asset import to the target table, and the CMDB Server.

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

{{< figure src="/images/automate/snow_integration_field_map.png" alt="Table Transaction Map">}}

#### Transform Map Scripts

The **Transformation** events occur while transforming an import set table into a target table. You can navigate to this script by selecting **Importing Data** >**Transform Event Scripts**.

- **On Before:** Mapping manufacturer, CPU manufacturer, model category, and model by using this script.

- **On After:** The  five **On After** scripts required to update the various values are as follows:

    1. Update **node id**, **node name**, **organization**, **IP address** and insert the record into Chef Automate client run cookbooks, Chef Automate client run recipes, Chef Automate client run lists.
    2. Update **software**, **name** and installed in _CMDB software instance_ table.
    3. Update data in the _cmdb\_ci\_spkg_ table and the _cmdb\_software\_instance_ table.
    4. Insert or update data in the _cmdb\_ci\_file\_system_ table.
    5. Insert or update data in the _cmdb\_ci\_network\_adapter_ table. 

{{< figure src="/images/automate/snow_integration_transform_map_scripts.png" alt="Transform Scripts">}}

##### *Script Includes:*

- **BufferToImportUtil:** Updates the CPU speed in Linux server, CMDB server, CMDB OSX server, and CMDB win server.
- **Logging:** Updates logging status as **enabled** or **disabled**.
- **JsonUtil:** Updates the JSON objects.
- **AutomateApi:** Calls the Chef Automate API and checks the status of the response.
- **AutomateApiClient:** Updates the token and password in the Chef Automate instance.
- **PropertiesUtil:** Updates the role in the property.
- **ImportUtil:** Updates the compliance import data.
- **Util:** Updates manufacturer data in the core company table and updates the category and manufacturer data in the _CMDB model_ table.

{{< figure src="/images/automate/snow_integration_transform_map_scripts1.png" alt="Script Includes">}}

## Configuration

You can configure Chef Automate by following two methods:

- Using the ServiceNow application
- Using the Chef Automate UI

{{ < note > }}

Ensure you set up the data imports using Chef Automate's **Data Feed** feature to avoid unnecessary performance impacts to your ServiceNow infrastructure. Initially, you can configure `feed_interval` setting to the larger time intervals, for example, **4 or 8 hours**, and with a smaller `node_batch_size` of **50**. 

In addition, you can test it on your development infrastructure by using the **CIDR Filter** functionality. Thus, the Chef Automate's **Data Feed** feature may suit your production environment, depending on the size of your estate and the setup you made in the ServiceNow production environment.

{{ < /note > }}

### Configuring Chef Automate from ServiceNow

The Chef Automate configuration from a ServiceNow production instance requires the Chef Automate instance with a valid signed SSL certificate.

Follow these steps to configure the Chef Automate integration from ServiceNow:

1. Navigate to the **Chef Automate** menu.
2. Select the **Automate Instances** module.
3. Click the **Submit** button to create a new Automate instance.
4. Enter the following details:
   
   - Name: a unique name for the integration.
   - URL: URL of the Chef Automate instance.
   - Chef automate API token: an API token generated for Chef Automate with data-feed-service authorization.
   - ServiceNow user: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - ServiceNow password: password of the ServiceNow user.
  
5. Click the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes. A successful test displays the **service-now automate connectivity passed** message. If there is an error, a message is displayed that will help resolve any connectivity or credentials issues. 
6. Click **Submit**. The Automate instance details are saved.

### Configuring Chef Automate from Chef Automate UI

Follow these steps to configure the Chef Automate integration from the Chef Automate UI:

1. Navigate to the **Settings** menu.
1. Click the **Data Feeds** link from the **General Settings**.
1. Click the **Create Data Feed** button to create a Data Feed.
1. Enter the following details:
   
   - Name: a unique name for the integration.
   - Data Feed URL: URL of the ServiceNow application Datafeed API ending `api/x_chef_automate/asset`.
   - Chef Automate API token: an API token generated for Chef Automate with data-feed-service authorization.
   - ServiceNow user: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - ServiceNow password: Password of the ServiceNow user.
  
   Note that to send data from Chef Automate to ServiceNow, you need to specify the **Data Feed URL** in the Chef Automate server `FQDN/api/x_chef_automate/asset` (Fully Qualified Domain Name (FQDN) is the domain name of the ServiceNow instance to configure in Chef Automate). For example, <https://venxxx.service-now.com/api/x_chef_automate/asset>.

2. Click the **Test Data Feed** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes. A successful test displays the **service-now automate connectivity passed** message. If there is an error, a message is displayed that will help resolve any connectivity or credentials issues. 
3. Click **Create Data Feed**. The configuration is saved.
   
{{< figure src="/images/automate/snow_integration_create_data_feed.png" alt="Create Data Feed">}}

## Navigation

In ServiceNow, the navigation of the application is through the **Chef Automate** menu.

{{< figure src="/images/automate/snow_integration_navigation.png" alt="Navigation">}}

The **Automate Instances** module allows the user to configure the integration with Chef Automate. The **Server** module displays a list of servers in the _CMDB_ module. 

The Chef Automate integration augments the existing CMDB servers and inserts new servers into CMDB. The application uses the ServiceNow Discovery IRE (Identification and Reconciliation Engine) when inserting or updating servers.

In addition, the application updates the CMDB file systems and software installed tables and adds related information on the servers with associated data from Chef Automate:

- Client Runs
- Attributes
- Compliance Reports

### Client Runs

The user can drill down into **Client Run** detail from a server record by clicking the name on an individual Client Run. Client Run record displays related information for:

- Client run cookbooks - cookbooks executed during the client run.
- Client run lists - run lists executed during the client run.
- Client run recipes - recipes executed during the client run.

The entire Chef Client Run details are available for each server. In addition, Client runs are also available from the **Client Runs** module.

### Attributes

The user can drill down into the current server attributes detail from a server record by clicking on the attributes record. The entire OHAI attributes are available for each server.

### Compliance Reports

The user can drill down into **Compliance** report detail from a server record by clicking the name on an individual **Compliance** report. The Compliance Report record displays related information for:

- Compliance report profiles - all profiles executed during the compliance scan.
- Compliance report results - all results from the compliance scan.

In addition, the user can drill down into each **Compliance** report profile to view the individual results for each profile. The entire Chef Compliance Report details are available for each server. Compliance reports are also available from the **Compliance** report module.

## Uninstalling

To uninstall the application:

1. In the ServiceNow instance, navigate to the **System Applications** > **Applications** menu.
2. From the **Downloads** tab, select the **Chef Automate** link.
3. In the **Related Links** section, select **Uninstall**. 

