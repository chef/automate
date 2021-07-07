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

The installation personnel performing the task of installing and configuring the Chef Automate Integration app must be aware of the ServiceNow ecosystem.

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
6. Select **Install**.

### Creating Application Users

The Chef Automate application provides several roles appropriate for integration. You can assign the following roles to the existing or new ServiceNow users:

[Role x_chef_automate.admin]({{< relref "#role_x_chef_automate.admin" >}}))
[Role x_chef_automate.user]({{< relref "#role_x_chef_automate.user" >}})
[Role x_chef_automate.api]({{< relref "#role_x_chef_automate.api" >}})

These roles are part of the package. You can create the requisite roles and controls if any further restrictions are needed.

## Configuration

You can configure Chef Automate either using ServiceNow or using the Chef Automate UI.

{{< note >}}

- You can configure `feed_interval` setting to **2 or 8 hours** based on the number of nodes. If more nodes are required, then you must increase the `feed_interval`.
- The benchmark with `node_batch_size` set as **15** is tested. You can set the value between **1** and **30**.
- The size of Scans can be large as multiple profiles are run. Hence, you must retain the `node_bach_size` smaller to avoid **4MB GRPC** limits exceeding.

{{< /note >}}

### Configuring Chef Automate from ServiceNow

The Chef Automate configuration from a ServiceNow production instance requires the Chef Automate instance with a valid signed SSL certificate.

Follow these steps to configure the Chef Automate integration from ServiceNow:

1. Navigate to the **ServiceNow** instance.
2. Locate **Chef Automate** menu.
   Alternatively, you can type **Chef** in the **Filer Navigator** text box, and the related Chef features display within **Chef Automate** menu.
3. Select the **Automate Instances** module.
4. Select the **New** button.
5. Enter the following details:

   - **Name**: a unique name for the integration.
   - **Instance URL**: URL of the Chef Automate instance.
   - **Automate API token**: an API token generated for Chef Automate with data-feed-service authorization.
   - **ServiceNow user**: a ServiceNow user with **ITIL** and `x_chef_automate.api` roles.
   - **ServiceNow password**: password of the ServiceNow user.

6. Select the **Test Connectivity** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes. A successful test displays the **service-now automate connectivity passed** message. If there is an error, a message is displayed that will help resolve any connectivity or credentials issues.
7. Select **Submit**. The Automate instance detail is created.

### Configuring Chef Automate from Chef Automate UI

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

5. Select the **Test Data Feed** button. The application checks that the values specified are correct and the connectivity between Chef Automate and the application establishes. A successful test displays the **service-now automate connectivity passed** message. If there is an error, a message is displayed that will help resolve any connectivity or credentials issues.
6. Select **Create Data Feed**. The data feed is created.

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
| `x_chef_automate.logging.verbosity` | Debugs the data in ServiceNow. The possible values are:
<ul>
<li>Debug</li>
<li>Warn</li>
<li>Info</li>
<li>Error</li>
</ul>
It enables the selected logging level and is visible in logs. | Default: `Error` |
| `x_chef_automate.Rest.api` | Enables the Chef Automate API from ServiceNow when Turn on REST API is set to `Yes`. The possible values are: Yes, No. | Default: `Yes` |
| `x_chef_automate.enable.system.app` | Used to enable software installed mappings. | Default: `No` |

3. Make the required changes.
4. Select Save. The application saves the configuration changes.

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

The user can drill down into **Client Run** detail from a server record by selecting the name on an individual Client Run. Client Run record displays related information for:

- Client run cookbooks: cookbooks executed during the client run.
- Client run lists: run lists executed during the client run.
- Client run recipes: recipes executed during the client run.

The entire Chef Client Run details are available for each server. In addition, Client runs are also available from the **Client Runs** module.

### Attributes

The user can drill down into the current server attributes detail from a server record by selecting on the attributes record. The entire OHAI attributes are available for each server.

### Compliance Reports

The user can drill down into **Compliance** report detail from a server record by selecting the name on an individual **Compliance** report. The Compliance Report record displays related information for:

- Compliance report profiles: all profiles executed during the compliance scan.
- Compliance report results: all results from the compliance scan.

In addition, the user can drill down into each **Compliance** report profile to view the individual results for each profile. The entire Chef Compliance Report details are available for each server. Compliance reports are also available from the **Compliance** report module.

### Benchmarking

The Chef Automate Installation team has tested the integration of the ServiceNow app with a maximum of 10K nodes Client Run data. Beyond this range, there might be performance issues. Also, the performance may get affected if you have any other applications running in your environment.

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
