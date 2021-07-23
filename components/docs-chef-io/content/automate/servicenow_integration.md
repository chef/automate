+++
title = "ServiceNow Integration"

draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "ServiceNow Integration"
    parent = "automate/reference"
    identifier = "automate/reference/servicenow_integration.md ServiceNow Integration"
    weight = 50
+++

## Chef Automate Integration with CMDB App in ServiceNow

[ServiceNow](https://www.servicenow.com/) provides cloud-based services that automate enterprise IT operations. ServiceNow specializes in IT service management (ITSM) applications and provides forms-based workflow application development. It supports third-party application and data integrations. The most common integrations are configuration management database (CMDB), incident management, problem management, change management, user administration, and single sign-on authentication.

The Chef Automate plugin for ServiceNow enables the integration of Chef Automate in the ServiceNow ecosystem. The Chef Automate integration is a ServiceNow-certified scoped application available from the ServiceNow store. It integrates existing Chef Automate infrastructure and compliance functionality with ServiceNow enterprise services. Once installed and configured, this integration allows Chef Automate to create and update a ServiceNow Configuration Management Database (CMDB) with data from nodes managed by Chef Automate.

{{< figure src="/images/automate/snow_integration_dataflow_diagram.png" alt="Data Flow Diagram">}}

The Chef Automate integration app plugin enables the Chef Automate to create and update client node records to Configuration Management Database (CMDB) tables of ServiceNow. This data aids in aggregating information about client nodes, the client runs, and compliance scans. The client runs and compliance scans monitors the state of configuration and compliance policies of your nodes.

## Installation

The [Chef Automate](https://www.chef.io/automate/) provides a full suite of enterprise capabilities for workflow, node visibility, and compliance. The integration between a Chef Automate server and a ServiceNow instance requires the following components:

- Chef Automate Scoped Application
- Chef Automate Server

The installation personnel performing the task of the Chef Automate integration in the ServiceNow ecosystem must have basic understanding of the ServiceNow system.

### Prerequisites

#### System Requirements

The ServiceNow instance must be publicly reachable on https port 443.

#### Software Requirements

- A running [Chef Automate](https://docs.chef.io/chef_automate.html) instance.
- A running [ServiceNow](https://www.servicenow.com/) instance

#### Required ServiceNow Plugins

The following ServiceNow plugins are mandatorily installed before you proceed to install Chef Automate instance in ServiceNow:

- System Import Sets `com.glide.system_import_set`, min version 1.0.0.
- Configuration Management (CMDB) 1.1.
- Configuration Management for Scoped Apps (com.snc.cmdb.scoped) 1.0.0.

{{< note >}}

You can locate the **System Import Sets**, **CMDB 1.1**, and **Configuration Management for Scoped App** plugins by navigating to the **System Applications** > **All Available Applications** > **All** section in the ServiceNow application.

{{< figure src="/images/automate/snow_integration_plugins.png" alt="Plugins">}}

{{< /note >}}

### Install the Chef Automate ServiceNow application

The Chef Automate application exposes the REST API endpoint that facilitates the communication between Chef Automate and the ServiceNow instance.  The Chef Automate server sends HTTPS JSON data feeds to the Chef Automate application in a ServiceNow instance to create and update the _ServiceNow CMDB_ tables, the application client run, and compliance report tables.

1. Visit the ServiceNow store at <https://store.servicenow.com>.
2. Search for **Chef Automate**.
3. Select the **Chef Automate Integration App**.
4. Select **Get** and follow the instructions by specifying your ServiceNow credentials.
5. Open your **ServiceNow Service Management** application
6. Select **System Applications** > **All Available Applications** > **All** menu.
7. Find the application using the filter criteria and search bar.
   Note that you can search for the application by its Name or ID. If you cannot find an application, you may have to request it from the ServiceNow Store.
8. Select **Install**.

### Associate Default User Roles

{{< note >}}

You must have the `x_chef_automate.api` role assigned to configure Chef Automate in ServiceNow and receive data from Chef Automate to ServiceNow instance.

{{< /note >}}

The Chef Automate application provides the following roles by default for integration in ServiceNow:

- [Role x_chef_automate.admin]({{< relref "#role_x_chef_automate.admin" >}}))
- [Role x_chef_automate.user]({{< relref "#role_x_chef_automate.user" >}})
- [Role x_chef_automate.api]({{< relref "#role_x_chef_automate.api" >}})

You can assign these roles to the existing or new ServiceNow users. These roles are part of the package. If there is a need for further restrictions, you can create the required roles and controls. Also, you can associate a single user with more than one role.

Refer to the following pages on how to create users and assign roles in ServiceNow:

- [Creating Users in ServiceNow](https://docs.servicenow.com/csh?topicname=t_CreateAUser.html)
- [Assigning Roles in ServiceNow](https://docs.servicenow.com/csh?topicname=t_AssignARoleToAUser.html)

## Configuration

You can configure Chef Automate either using the ServiceNow application or using the Chef Automate UI.

{{< note >}}

- You can configure `feed_interval` setting to **2 or 8 hours** based on the number of nodes. If there is a need to configure more nodes, you must increase the `feed_interval`.
- The testing executed for a benchmark with `node_batch_size` set as **15**. You can set the value between **1** and **30**.
- The size of scans can be significant to execute profiles in large numbers. Hence, you must maintain the `node_bach_size` smaller to avoid **4MB GRPC** limits exceeding.

{{< /note >}}

### Configuring Chef Automate from ServiceNow

The Chef Automate configuration from a ServiceNow production instance requires the Chef Automate instance with a valid signed SSL certificate.

{{< note >}}

Ensure you have the `x_chef_automate.api` role assigned to perform the Chef Automate configuration.

{{< /note >}}

Follow these steps to configure the Chef Automate integration from ServiceNow:

1. Navigate to the **ServiceNow** instance.
2. Locate **Chef Automate** menu.
   Or, you can type **Chef** in the **Filer Navigator** text box, and the Chef Automate features display within **Chef Automate** menu.
3. Select the **Automate Instances** module.
4. Select the **New** button.
5. Enter the following details:

   - **Name**: a unique name for the integration.
   - **Instance URL**: URL of the Chef Automate instance.
   - **Automate API token**: an API token generated for Chef Automate with data-feed-service authorization.
   - **ServiceNow user**: a ServiceNow user name with **ITIL** and `x_chef_automate.api` roles.
   - **ServiceNow password**: password of the ServiceNow user.

6. Select the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes.

   A successful test displays the **service-now automate connectivity passed** message. Else, an error message displays detailing the connectivity or credentials issues and help you to resolve.

7. Select **Submit**. The ServiceNow app creates the Automate instance detail.

{{< figure src="/images/automate/snow_integration_configure_sn.png" alt="Automate Instances">}}

### Configuring Chef Automate from Chef Automate UI

{{< note >}}

Ensure you have the `x_chef_automate.api` role assigned to perform the Chef Automate configuration.

{{< /note >}}

Follow these steps to configure the Chef Automate integration from the Chef Automate UI:

1. Navigate to the **Settings** from the **Chef Automate UI** menu.
2. Select the **Data Feeds** link from the **Settings**.
3. Select the **Create Data Feed** button.
4. Enter the following details:

   - **Name**: a unique name for the integration.
   - **Data Feed URL**: URL of the ServiceNow application Datafeed API ending `api/x_chef_automate/asset`.
   - **Username**: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - **Password**: Password of the user.

   Note that to send data from Chef Automate to ServiceNow, you need to specify the **Data Feed URL** in the Chef Automate server `FQDN/api/x_chef_automate/asset` (Fully Qualified Domain Name (FQDN) is the domain name of the ServiceNow instance to configure in Chef Automate). For example, <https://venxxx.service-now.com/api/x_chef_automate/asset>.

5. Select the **Test Data Feed** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes.

   A successful test displays the **service-now automate connectivity passed** message. Else, an error message displays detailing the connectivity or credentials issues and helps you in resolving the error.

6. Select **Create Data Feed**. The application creates the data feed.

{{< figure src="/images/automate/snow_integration_create_data_feed.png" alt="Create Data Feed">}}

### Configure Application Properties

You must have the `admin` or `x_chef_automate.admin` roles assigned to change the default values in the Application Properties section if required.

1. Navigate to the **ServiceNow** application.
1. Select the **Chef Automate** > **Properties** option from the left navigation pane. The **Chef Automate Properties** screen appears.

   {{< figure src="/images/automate/snow_integration_appproperties.png" alt="Chef Automate Properties">}}

   The application system properties are:

   | Property Name | Description | Default |
   | --------------| ----------  | --------|
   | `x_chef_automate.chef.default.status` | Used to set up the status of the service record as **inserted** or **updated**. | Default: `1`|
   | `x_chef_automate.client_runs_per_node` | Used to set a highest number of clients runs for a node. | Default: `5` |
   | `x_chef_automate.compliance_reports_per_node` | Used to set a highest number of compliance reports for a node. | Default: `5` |
   | `x_chef_automate.insert_manufacturer` | Inserts the new record during the import if a model is not found in the _core\_company_ table by setting the property to **Yes**. | Default: `Yes` |
   | `x_chef_automate.insert_model` | Inserts the new record during the import if a model is not found in the _cmdb\_model_ table by setting the property to **Yes**. | Default: `Yes` |
   | `x_chef_automate. logging.enabled` | Used to flag the logging with **enable** or **disable** values. | Default: `No` |
   | `x_chef_automate.logging.verbosity` | Debugs the data in ServiceNow. The possible values are: <ul><li> Debug </li> <li> Warn </li> <li> Info </li> <li> Error </li> </ul>It enables the selected logging level and is visible in logs. | Default: `Error` |
   | `x_chef_automate.Rest.api` | Enables the Chef Automate API from ServiceNow when Turn on REST API set to `Yes`. The possible values are: Yes, No. | Default: `Yes` |
   | `x_chef_automate.enable.system.app` | Used to enable software installed mappings. | Default: `No` |

1. Make the required changes.
1. Select Save. The application saves the configuration changes.

## Chef Automate in ServiceNow

Once the successful installation and configuration of the Chef Automate plugin in ServiceNow completes, you can view the **Chef Automate** menu in ServiceNow.

{{< figure src="/images/automate/snow_integration_navigation.png" alt="Navigation">}}

The **Automate Instances** module allows the user to configure the integration with Chef Automate. The **Server** module displays a list of servers in the _CMDB_ module.

The Chef Automate integration augments the existing CMDB servers and inserts new servers into CMDB. The application uses the ServiceNow Discovery IRE (Identification and Reconciliation Engine) when inserting or updating servers.

Also, the application updates the CMDB file systems and software installed tables and adds related information on the servers with associated data from Chef Automate:

- Client Runs
- Attributes
- Compliance Reports

### Client Runs

You can view the **Client Run** detail from a server record by selecting the name on an individual Client Run. Client Run record displays related information for:

- Client run cookbooks: cookbooks executed during the client run.
- Client run lists: run lists executed during the client run.
- Client run recipes: recipes executed during the client run.

The entire Chef Client Run details are available for each server. Also, Client runs are available from the **Client Runs** module.

### Attributes

You can view the current server attributes detail from a server record by selecting the attributes record. The entire OHAI attributes are available for each server.

### Compliance Reports

You can view the **Compliance** report detail from a server record by selecting the name on an individual **Compliance** report. The Compliance Report record displays related information for:

- Compliance report profiles: all profiles executed during the compliance scan.
- Compliance report results: all results from the compliance scan.

Also, you can view the **Compliance** report profile to display the individual results for each profile. The entire Chef Compliance Report details are available for each server. Compliance reports are also available from the **Compliance** report module.

## Benchmarking

The Chef Automate Installation team has tested the integration of the ServiceNow app with a maximal of 10K nodes Client Run data. Beyond this range, there might be performance issues. Also, the performance may get affected if you have any other applications running in your environment.

The typical production instance of ServiceNow can have 8-12 nodes, which would mean the performance will be +10K nodes per hour processed with client run data. Benchmarking indicates that the increase in the number of nodes in the ServiceNow instance reduces the processing time.

| Client Nodes  | Total Time Taken (Dev 1 Node)  | Total Time Taken (Vendor 2 Node)  |
|---------------|--------------------------------|-----------------------------------|
| 200           | 7m48s                          | 4m4s                              |
| 500           | 17m47s                         | 10m16s                            |
| 2000          | 1h15m                          | 45m                               |
| 10,000        | 5h40m                          | 3h3m                              |

## Uninstalling

To uninstall the application:

1. In the ServiceNow instance, navigate to the **System Applications** > **Applications** menu.
2. From the **Downloads** tab, select the **Chef Automate** link.
3. In the **Related Links** section, select **Uninstall**.

Refer to the [ServiceNow Integration Reference]({{< relref "servicenow_integration_addendum" >}}) page to find information on the topics that will help you while installing and configuring Chef Automate in ServiceNow.
