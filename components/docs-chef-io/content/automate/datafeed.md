+++
title = "Data Feeds"
date = 2020-05-05T13:19:02-07:00
draft = false

gh_repo = "automate"

[menu]
  [menu.automate]
    title = "Data Feeds"
    parent = "automate/settings"
    identifier = "automate/settings/datafeed.md Data Feeds"
    weight = 20
+++

The Data Feed service sends node data to a 3rd party service.
This can be useful when updating configuration management databases, external security dashboards and IT service management platforms.
The following types of information are sent:

- Ohai data gathered from each managed node - This data includes hardware, operating system, and installed program information. Some variation depends on the managed operating system
- Configuration information about each managed node - This information includes Chef Client Run status, Runlists, Cookbooks, and Recipes being ran against each node
- Compliance information about each node that shows the compliance state - This information includes passed and failed controls for each profile executed against that node

A Data Feed operates by doing the following:

- Every 4 hours, the data-feed-service will aggregate the client runs and compliance reports from the previous 4 hours and send this information to the registered destinations. This time interval is 4 hours by default, but is configurable
- If there are no destinations, aggregation will not occur
- The data aggregates and sends in batches of 50 nodes at a time. The batch amount is 50 by default, but is configurable

By default, only Admin users of Chef Automate may create and manage Data Feeds.

## Data Feed Integration

Data Feed instance sends client run and compliance scan data to the 3rd party integrations available. To add a Data Feed instance in Chef Automate:

1. In the **Settings** tab, navigate to _Data Feeds_ in the sidebar
1. Select **New Integration**

Currently, the data feed has two types of integrations:

- Webhook Integration
  - ServiceNow
  - Splunk
  - ELK
  - Custom
- Storage Integration
  - Minio
  - Amazon S3

{{< figure src="/images/automate/choose-a-data-feed-integration.png" alt="Choose a Data Feed Integration">}}

## Webhook Integration

Create a data feed instance using any webhook integration.

### ServiceNow

To add a Data Feed instance in Chef Automate using ServiceNow integration:

1. Select **New Integration**.

1. Select the **ServiceNow** icon under Webhook Integration.

1. In the form, enter a unique **Data Feed name**.

1. Enter a **URL** for the Data Feed endpoint, including any specific port details.

1. Select an **authentication** from the drop-down. (For example, we are using _Username and Password_)

1. Enter the **Username** and **Password** that your 3rd party endpoint requires for authentication.

1. Select **Test Connection** to start validating the connection details.

1. Once the test is successful, select **Save** to save the Data Feed configuration.

{{< figure src="/images/automate/data-feed-instance-using-servicenow-integration.png" alt="Data Feed Instance using ServiceNow Integration">}}

#### Edit a Data Feed Instance

To edit a Data Feed instance of ServiceNow Integration:

1. Select the data feed instance name to open its detail page.

1. Edit the Data Feed **Name** or **URL**.

1. Select **Test Data Feed** button to test the URL.

1. Select **Save** to save your changes.

{{< figure src="/images/automate/details-of-data-feed-instance-using-servicenow-integration.png" alt="Details of Data Feed Instance using ServiceNow Integration">}}

You can also **Enable**, **Disable** and **Delete** the instance from the buttons provided on the details page.

#### Delete a Data Feed Instance

To delete an individual instance:

1. Selecting the ellipses icon {{< fontawesome class="fas fa-ellipsis-h" >}}.

1. Select **Delete**.

{{< figure src="/images/automate/delete-the-data-feed-instance-using-servicenow-integration.png" alt="Delete the Data Feed Instance using ServiceNow Integration">}}

1. In the popup, select **Delete Data Feed**.

{{< figure src="/images/automate/delete-data-feed-popup-of-data-feed-instance-using-servicenow-integration.png" alt="Delete Data Feed Popup of Data Feed Instance using ServiceNow Integration">}}

## Configuring Global Data Feed Behavior

{{< note >}}
The Data Feed configuration settings apply across all configured Data Feed instances.
{{< /note >}}

To modify Data Feed behavior with the available configuration settings:

1. Create a configuration patch file to update the configuration settings. Save this file in the `.toml` file format and name your file as desired. For example, `data-feed-patch.toml`

1. Include one or more configuration settings and their updated value(s) in your configuration patch `.toml` file to reflect the desired global Data Feed behavior:

  - Use the `feed_interval` setting to change the interval for the Data Feed collection. The default value is four hours
  - Use the `node_batch_size` setting to change the number of sets of node data sent in each individual batch to your endpoint. The default value is 50 nodes
  - Use the `updated_nodes_only` setting to determine what data to include in each export. The default setting is `true`, which causes the aggregation of only the *changed* data of updated nodes since the last export. Set `updated_nodes_only` to `false` and it aggregates *all* data of updated nodes since the last export
  - To reduce the IP address range for the collected and processed node data, update the `disable_cidr_filter` setting to `false` **and** update the `cidr_filter` setting to cover the required IP address range. For example, you may wish to send only production or test node traffic
  - Use the `accepted_status_codes` setting to define an array of HTTP status codes that the Data Feed Service will treat as `success` if returned by the 3rd party endpoint. If the status code is not in the `accepted_status_codes` list, then an error will be logged

1. Save your configuration patch file changes before continuing to the next step.

1. Apply your configuration changes with the Chef Automate command-line tool:

```bash
    chef-automate config patch data-feed-patch.toml
```

    where `data-feed-patch.toml` is this example's configuration patch file.

### Configuration Patch File Example

```toml
[data_feed_service.v1.sys]
  [data_feed_service.v1.sys.service]
        feed_interval = "4h"
        node_batch_size = 50
        updated_nodes_only = true
        disable_cidr_filter = true
        cidr_filter = "0.0.0.0/0"
        accepted_status_codes = [200, 201, 202, 203, 204]
      [data_feed_service.v1.sys.log]
        level = "info"
```

To debug any issues with the Data Feed Service in Chef Automate, update the following section in your configuration patch file by changing the `log_level` value to "debug":

```toml
    [data_feed_service.v1.sys.log]
    log_level = "debug"
```

## Data Feed Output Syntax and Details

The outputted data from Data Feed consists of line-separated JSON strings.
Each line represents the data for one node, and contains the following properties:

```json
    {
    "attributes": {
     "node_id": "",
     "name": "",
     "run_list": [],
     "chef_environment": "",
     "normal": {},
     "default": {},
     "override":{},
     "automatic":{},
     "normal_value_count": 0,
     "default_value_count": 1,
     "override_value_count": 1,
     "all_value_count": 10,
     "automatic_value_count": 8
    },
    "report": { ... },
    "client_run": { ... },
    "node": {
     "automate_fqdn": "",
     "ip_address" : "",
     "mac_address": "",
     "description":"",
     "serial_number":"",
     "os_service_pack":""
     }
    }
```
