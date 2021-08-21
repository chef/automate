+++
title = "Chef Automate Integration App"
draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Chef Automate Integration App"
    parent = "automate/reference"
    identifier = "automate/reference/servicenow_integration.md ServiceNow Integration"
    weight = 50
+++

[ServiceNow](https://www.servicenow.com/) provides cloud-based services that automate enterprise IT operations. ServiceNow specializes in IT service management (ITSM) applications and provides forms-based workflow application development. It supports third-party application and data integrations. The most common integrations are for configuration management, incident management, problem management, change management, user administration, and single sign-on authentication.

The Chef Automate Integration App for ServiceNow integrates Chef Automate with ServiceNow. This app is a ServiceNow-certified scoped application available from the ServiceNow store. It integrates existing Chef Automate infrastructure and compliance functionality with ServiceNow enterprise services. Once installed and configured, this integration enables Chef Automate to create and update a ServiceNow Configuration Management Database (CMDB) with data from nodes managed by Chef Automate. Chef Automate aggregates information about infrastructure nodes, the Chef Infra Client runs, and Chef Compliance scans, helping you monitor your infrastructure in real time.

{{< figure src="/images/automate/snow_integration_dataflow_diagram.png" alt="Data Flow Diagram" >}}

## Installation

[Chef Automate](/automate/) provides a full suite of enterprise capabilities for workflow, node visibility, and compliance. The integration between a Chef Automate server and a ServiceNow instance requires the following components:

- Chef Automate Scoped Application
- Chef Automate Server

The installation personnel performing the task of the Chef Automate Integration App in the ServiceNow ecosystem must have basic understanding of the ServiceNow system.

### System Requirements

The ServiceNow instance must be reachable on port 443 from Chef Automate.

### Software Requirements

- A running [Chef Automate](https://www.chef.io/automate/) instance.
- A running [ServiceNow](https://www.servicenow.com/) instance.

### Required ServiceNow Plugins

Install following ServiceNow plugins:

- System Import Sets `com.glide.system_import_set`, min version 1.0.0.
- Configuration Management (CMDB) 1.1.
- Configuration Management for Scoped Apps (com.snc.cmdb.scoped) 1.0.0.

Locate the **System Import Sets**, **CMDB 1.1**, and **Configuration Management for Scoped App** plugins by navigating to the **System Applications** > **All Available Applications** > **All** section in the ServiceNow application.

{{< figure src="/images/automate/snow_integration_plugins.png" alt="Plugins" >}}

### Install the Chef Automate Integration App

The Chef Automate Integration App exposes the REST API endpoint for communication between Chef Automate and the ServiceNow instance. Chef Automate sends HTTPS JSON data feeds to the Chef Automate Integration App in a ServiceNow instance to create and update the _ServiceNow CMDB_ tables, client run, and compliance report tables.

1. Navigate to the ServiceNow store at <https://store.servicenow.com>.
2. Search for **Chef Automate**.
3. Select the **Chef Automate Integration App**.
4. Select **Get** and follow the instructions by specifying your ServiceNow credentials.
5. Open your **ServiceNow Service Management** application.
6. Select **System Applications** > **All Available Applications** > **All** menu.
7. Find the application using the filter criteria and search bar.
   You can search for the application by its `Name` or `ID`. If you cannot find an application, you may have to request it from the ServiceNow Store.
8. Select **Install**.

### Assign User Roles

The ServiceNow users must have the `x_chef_automate.api` role to set up the ServiceNow instance to receive data from Chef Automate. Changing the **Application Properties** defaults requires the `x_chef_automate.admin` role.

The Chef Automate Integration App has three default roles:

- [Role x_chef_automate.admin]({{< relref "servicenow_integration_reference#role-x_chef_automateadmin" >}})
- [Role x_chef_automate.user]({{< relref "servicenow_integration_reference#role-x_chef_automateuser" >}})
- [Role x_chef_automate.api]({{< relref "servicenow_integration_reference#role-x_chef_automateapi" >}})

You can assign these roles to the existing or new ServiceNow users. These roles are part of the package. If there is a need for further restrictions, you can create the required roles and controls. Also, you can associate a single user with more than one role.

The `x_chef_automate.user` role is suitable for those users who require application access without administration rights. To alter the system properties, you need to have the `x_chef_automate.admin` role.

Refer to the following pages on how to create users and assign roles in ServiceNow:

- [Creating Users in ServiceNow](https://docs.servicenow.com/csh?topicname=t_CreateAUser.html)
- [Assigning Roles in ServiceNow](https://docs.servicenow.com/csh?topicname=t_AssignARoleToAUser.html)

### Discovery Source

The Chef Automate Integration App provides a unique ID that distinguishes between discovered Configuration Items (CIs) and imported CIs. CIs discovered by the Integration App take a value of **Chef Automate** by default in the **Discovery Source** field part of the `cmdb_servers` table.

{{< figure src="/images/automate/snow_integration_discovery.png" alt="Discovery Source">}}

## Configuration

You can configure Chef Automate Integration App from your ServiceNow or Chef Automate instance.

`feed_interval`
: The frequency in hours for refreshing the data feed. The duration between data feed refreshes is proportional to the node count, with more nodes requiring higher settings. Valid values: Any integer in the range of `2` to `8`. Default: `4`.

`node_batch_size`
: The testing executed for a benchmark. The size of a compliance scan is proportional to the number of profiles applied. Scans exceeding 4MB may fail or display incorrectly in reports. Use a lower `node_batch_size` setting to reduce the number of profiles applied in a single batch. Valid values: Between `1` and `30`.  Default: `15`.

### Configure Chef Automate from ServiceNow

{{< warning >}}
Configuring Chef Automate Integration App from a ServiceNow production instance requires a valid SSL certificate from a trusted certificate authority (CA).
{{< /warning >}}

Follow these steps to configure the Chef Automate Integration App from ServiceNow:

1. Confirm that you have the `x_chef_automate.api` role.
1. Navigate to the **ServiceNow** instance.
1. Locate **Chef Automate** menu.
   Or, you can type **Chef** in the **Filer Navigator** text box, and the Chef Automate features display within **Chef Automate** menu.
1. Select the **Automate Instances** module.
1. Select the **New** button.
1. Enter the following details:

   - **Name**: a unique name for the integration.
   - **Instance URL**: URL of the Chef Automate instance.
   - **Automate API token**: an API token generated for Chef Automate with data-feed-service authorization.
   - **ServiceNow user**: a ServiceNow user name with **ITIL** and `x_chef_automate.api` roles.
   - **ServiceNow password**: password of the ServiceNow user.

1. Select the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes.

   A successful test displays the **service-now automate connectivity passed** message. Else, an error message displays detailing the connectivity or credentials issues and help you to resolve.

1. Select **Submit**. The ServiceNow app creates the Chef Automate instance detail.

{{< figure src="/images/automate/snow_integration_configure_sn.png" alt="Chef Automate Instances">}}

### Configure the Chef Automate Integration App from Chef Automate

Follow these steps to configure the Chef Automate Integration App from Chef Automate:

1. Confirm that you have the `x_chef_automate.api` role.
1. Navigate to **Settings** from the **Chef Automate** menu.
1. Select the **Data Feeds** link from **Settings**.
1. Select the **Create Data Feed** button.
1. Enter the following information:

   - **Name**: a unique name for the Chef Automate Integration App.
   - **Data Feed URL**: the URL of the ServiceNow application Datafeed API ending with `api/x_chef_automate/asset`.
   - **Username**: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - **Password**: Password of the user.

   You must specify the fully qualified domain name (FQDN) of the ServiceNow instance to configure the **Data Feed URL** in Chef Automate, which has the format: `FQDN/api/x_chef_automate/asset` . For example, <https://venxxx.service-now.com/api/x_chef_automate/asset>.

1. Select the **Test Data Feed** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes.

   A successful test displays the **service-now automate connectivity passed** message. Else, an error message displays detailing the connectivity or credentials issues and helps you in resolving the error.

1. Select **Create Data Feed**.

{{< figure src="/images/automate/snow_integration_create_data_feed.png" alt="Create Data Feed">}}

### Configure Application Properties

The Chef Automate Integration App has nine configurable **Application Properties**. You must have the `admin` or `x_chef_automate.admin` roles assigned to change the default values in the **Application Properties**.

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

Follow these steps to alter the default values in the **Application Properties** screen:

1. Navigate to the **ServiceNow** application.
1. Select the **Chef Automate** > **Properties** option from the left navigation pane. The **Chef Automate Properties** screen appears.

   {{< figure src="/images/automate/snow_integration_appproperties.png" alt="Chef Automate Properties">}}

1. Make the required changes.
1. Select **Save**.

## Chef Automate in ServiceNow

Once the installation and configuration of the Chef Automate Integration App in ServiceNow completes, you can view the **Chef Automate** menu in ServiceNow.

{{< figure src="/images/automate/snow_integration_navigation.png" alt="Navigation">}}

The **Automate Instances** module allows the user to configure the integration with Chef Automate. The **Server** module displays a list of servers in the **CMDB** module.

The Chef Automate Integration App augments the existing CMDB servers and inserts new servers into the ServiceNow CMDB. The Chef Automate Integration App uses the ServiceNow Discovery IRE (Identification and Reconciliation Engine) to insert or update servers.

Chef Automate Integration App updates the CMDB file systems and software tables. It adds associated data from Chef Automate to the servers for:

- Client Runs
- Attributes
- Compliance Reports

### Client Runs

You can view the data for a Chef Infra Client run by selecting a server from the list. The **Client Run** record displays information for:

- Client run cookbooks: cookbooks executed during the Chef Infra Client run.
- Client run lists: run lists executed during the Chef Infra Client run.
- Client run recipes: recipes executed during the Chef Infra Client run.

The complete Chef Infra Client run details are available for each server. Chef Infra Client runs are available from the **Client Runs** module.

### Attributes

You can view the current attributes for a server by selecting the attributes record. The entire OHAI attributes are available for each server.

### Compliance Reports

You can view the **Compliance** report detail from a server record by selecting the name on an individual **Compliance** report. The Compliance Report record displays related information for:

- Compliance report profiles: all profiles executed during the compliance scan.
- Compliance report results: all results from the compliance scan.

Also, you can view the **Compliance** report profile to display the individual results for each profile. The entire Chef Compliance Report details are available for each server. Compliance reports are also available from the **Compliance** report module.

## Benchmarking

The Chef Automate Installation team has tested the integration of the ServiceNow app with 10K nodes of Chef Infra Client Run data. Infrastructure with more than 10K nodes may have performance issues. The system performance will decrease with other applications running in your environment.

The typical production instance of ServiceNow can have between 8-12K nodes, and will perform at +10K nodes per hour processed with Chef Infra Client run data. Benchmarking indicates that the increase in the number of nodes in the ServiceNow instance reduces the processing time.

| Client Nodes  | Total Time Taken (Dev 1 Node)  | Total Time Taken (Vendor 2 Node)  |
|---------------|--------------------------------|-----------------------------------|
| 200           | 7m48s                          | 4m4s                              |
| 500           | 17m47s                         | 10m16s                            |
| 2000          | 1h15m                          | 45m                               |
| 10,000        | 5h40m                          | 3h3m                              |

## Uninstallation

To uninstall the Chef Automate Integration App:

1. In the ServiceNow instance, navigate to the **System Applications** > **Applications** menu.
1. Open the **Downloads** tab and select the **Chef Automate** link.
1. Navigate to the **Related Links** section.
1. select **Uninstall**.

Refer to the [ServiceNow Integration Reference]({{< relref "servicenow_integration_reference" >}}) page to find information on the topics that will help you while installing and configuring Chef Automate in ServiceNow.
