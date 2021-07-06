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


## Chef Automate Integration with CMDB App in ServiceNow

The Chef Automate integration app plugin enables the Chef Automate to create and update client node records to Configuration Management Database (CMDB) tables of ServiceNow. This data aids in aggregating information about client nodes, the client runs, and compliance scans. The client runs and compliance scans monitor the state of configuration and compliance policies of your nodes. 

## Chef Automate and ServiceNow Integration

The integration between a Chef Automate server and a ServiceNow instance requires the following components:

- Chef Automate Scoped Application
- Chef Automate Server

The Chef Automate application is a ServiceNow certified scoped application available from the ServiceNow store.  It integrates with the Chef Automate infrastructure and compliance functionality.  

The [Chef Automate](https://www.chef.io/automate/) provides a full suite of enterprise capabilities for workflow, node visibility, and compliance. The Chef Automate server sends HTTPS JSON data feeds to the Chef Automate application in a ServiceNow instance to create and update the _ServiceNow CMDB_ tables, the application client run and compliance report tables. 

{{< figure src="/images/automate/snow_integration_dataflow_diagram.png" alt="Data Flow Diagram">}}

## Installation
### Prerequisites

- The ServiceNow instance must be publicly reachable on https port 443. 
- [Chef Automate Server](https://docs.chef.io/chef_automate.html).
- ServiceNow plugin: System Import Sets `com.glide.system_import_set`, min version 1.0.0.
- ServiceNow plugin: Configuration Management (CMDB) 1.1.
- ServiceNow plugin: Configuration Management for Scoped Apps [com.snc.cmdb.scoped] 1.0.0.

The installation personnel performing the task of installing and configuring the Chef Automate Integration app is advised to be aware of the ServiceNow ecosystem.

{{< note >}}

You can obtain the **System Import Sets**, **CMDB 1.1**, and **Configuration Management for Scoped App** plugins by navigating to the **System Applications** > **All Available Applications** > **All** section in the ServiceNow application. Alternatively, you can also find them in the **Plugins** section.

{{< figure src="/images/automate/snow_integration_plugins.png" alt="Plugins">}}

{{< /note >}}

### Installing Chef Automate application in ServiceNow

The Chef Automate application exposes the REST API endpoint that facilitates the communication between Chef Automate and the ServiceNow instance.

1. Visit the ServiceNow store at <https://store.servicenow.com>.
2. Get the **Chef Automate Integration App** application.
3. Navigate to the ServiceNow instance.
4. Select **System Applications** > **All Available Applications** > **All** menu.
5. Find the application using the filter criteria and search bar.
   Note that you can search for the application by its Name or ID. If you cannot find an application, you may have to request it from the ServiceNow Store.
6. Click **Install**.

### Creating Application Users

The Chef Automate application provides several roles appropriate for integration. The existing or new ServiceNow users can be assigned these roles, which are as follows:

- [Role x_chef_automate.admin]({{< relref "Role x_chef_automate.admin" >}}) .
- [Role x_chef_automate.user]({{< relref "Role x_chef_automate.user" >}}) . 
- [Role x_chef_automate.user]({{< relref "Role x_chef_automate.user" >}}) .

These roles are part of the package. Users can create their requisite roles and controls if any further restrictions are needed.

## Configuration

You can configure Chef Automate either using ServiceNow or using the Chef Automate UI.

{{< note >}}

- You can configure `feed_interval` setting to **2 or 8 hours** based on the number of nodes. If more nodes are required, then you must increase the `feed_interval`.
- The benchmark with `node_batch_size` set as **15** is tested, however you can set the value between **1** and **30**. 
-  The size of Scans can be large as multiple profiles are run. Hence, you must ratain the `node_bach_size` smaller to avoid **4MB GRPC** limits being reached.

{{< /note >}}

### Configuring Chef Automate from ServiceNow

The Chef Automate configuration from a ServiceNow production instance requires the Chef Automate instance with a valid signed SSL certificate.

Follow these steps to configure the Chef Automate integration from ServiceNow:

1. Navigate to the **ServiceNow** instance.
2. Locate **Chef Automate** menu. 
   Alternatively, you can type **Chef** in the **Filer Navigator** text box, and the related Chef features display within **Chef Automate** menu.
3. Select the **Automate Instances** module.
4. Click the **New** button.
5. Enter the following details:
   
   - **Name**: a unique name for the integration.
   - **Instance URL**: URL of the Chef Automate instance.
   - **Automate API token**: an API token generated for Chef Automate with data-feed-service authorization.
   - **ServiceNow user**: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - **ServiceNow password**: password of the ServiceNow user.
  
6. Click the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes. A successful test displays the **service-now automate connectivity passed** message. If there is an error, a message is displayed that will help resolve any connectivity or credentials issues. 
7. Click **Submit**. The Automate instance detail is created.

### Configuring Chef Automate from Chef Automate UI

Follow these steps to configure the Chef Automate integration from the Chef Automate UI:

1. Navigate to the **Settings** from the **Chef Automate UI** menu.
2. Click the **Data Feeds** link from the **Settings**.
3. Click the **Create Data Feed** button.
4. Enter the following details:
   
   - **Name**: a unique name for the integration.
   - **Data Feed URL**: URL of the ServiceNow application Datafeed API ending `api/x_chef_automate/asset`.
   - **Username**: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - **Password**: Password of the user.

   Note that to send data from Chef Automate to ServiceNow, you need to specify the **Data Feed URL** in the Chef Automate server `FQDN/api/x_chef_automate/asset` (Fully Qualified Domain Name (FQDN) is the domain name of the ServiceNow instance to configure in Chef Automate). For example, <https://venxxx.service-now.com/api/x_chef_automate/asset>.

5. Click the **Test Data Feed** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes. A successful test displays the **service-now automate connectivity passed** message. If there is an error, a message is displayed that will help resolve any connectivity or credentials issues. 
6. Click **Create Data Feed**. The data feed is created.
   
{{< figure src="/images/automate/snow_integration_create_data_feed.png" alt="Create Data Feed">}}

### Configuring Application Properties

Users can configure the application properties with the `admin` or `x_chef_automate.admin` roles. 

1. Navigate to the **ServiceNow** application.
2. Select the **Chef Automate** > **Properties** option from the left navigation pane. The **Chef Automate Properties** screen is displayed.

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

3. Make the required changes.
4. Click Save. The application saves the configuration changes.

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

- Client run cookbooks: cookbooks executed during the client run.
- Client run lists: run lists executed during the client run.
- Client run recipes: recipes executed during the client run.

The entire Chef Client Run details are available for each server. In addition, Client runs are also available from the **Client Runs** module.

### Attributes

The user can drill down into the current server attributes detail from a server record by clicking on the attributes record. The entire OHAI attributes are available for each server.

### Compliance Reports

The user can drill down into **Compliance** report detail from a server record by clicking the name on an individual **Compliance** report. The Compliance Report record displays related information for:

- Compliance report profiles: all profiles executed during the compliance scan.
- Compliance report results: all results from the compliance scan.

In addition, the user can drill down into each **Compliance** report profile to view the individual results for each profile. The entire Chef Compliance Report details are available for each server. Compliance reports are also available from the **Compliance** report module.

### Benchmarking

The Chef Automate Installation team has tested the integration of the ServiceNow app with a maximum of 10K nodes Client Run data only. Beyond this range, there might be performance issues. Also, the performance may get affected if you have any other applications running in your environment.

The typical production instance of ServiceNow can have 8-12 nodes, which would mean the performance will be +10K nodes per hour processed with client run data. Benchmarking indicates that the increase in the number of nodes in the ServiceNow instance reduces the processing time.

| Client Nodes  | Total Time Taken (Dev 1 Node)  | Total Time Taken (Vendor 2 Node)  |
|---------------|--------------------------------|-----------------------------------|
| 200           | 7m48s                          | 4m4s                              |
| 500           | 17m47s                         | 10m16s                            |
| 2000          | 1h15m                          | 45m                               |
| 10,000        | 5h40m                          | 3h3m                              |

## Appendix/ Addendum

You can quickly list the Chef Automate features in the ServiceNow instance by typing **Chef** in the **Filer Navigator** text box. All Chef features are displayed within **Chef Automate** menu.

### Events

You can navigate to the **Events** section by selecting **Chef Automate** >**Events** from ServiceNow.

- The `x_chef_automate.chef.process.data` event is triggered whenever ServiceNow receives the node data from either terminal or through app.

{{< figure src="/images/automate/snow_integration_event_registration.png" alt="Event Registration - Process Data">}}

- The `x_chef_automate.chef.process.installation` event is triggered whenever the asset import table receives software installation details to map in the _CMDB server_ table.

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

The **Scripted Rest API** is used to establish a connection between ServiceNow and the Chef Automate application with authentication. You can navigate to the this section by selecting **Chef Automate** >**Script Rest API** from ServiceNow.

{{< figure src="/images/automate/snow_integration_scripted_restapi_1.png" alt="Scripted REST Service">}}

{{< figure src="/images/automate/snow_integration_scripted_restapi_2.png" alt="Scripted REST Resource">}}

### Roles

Refer the following pages on how to create users and assign roles in ServiceNow:

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

Note that for integration with CMDB data, you need to assign the _OOB ITIL_ role. In addition, you must select **Web service** access only for this user role.

## Uninstalling

To uninstall the application:

1. In the ServiceNow instance, navigate to the **System Applications** > **Applications** menu.
2. From the **Downloads** tab, select the **Chef Automate** link.
3. In the **Related Links** section, select **Uninstall**. 
