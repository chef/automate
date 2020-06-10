+++
title = "Data Feeds"
description = "Exporting Node Data from Chef Client Runs to External Data Collectors"
date = 2020-05-05T13:19:02-07:00
draft = false
bref = ""
toc = true
[menu]
  [menu.docs]
    parent = "settings"
    weight = 10
+++

## About Automate Data Feeds

The Data Feed service is used to send node data to a 3rd party service. This can be useful when updating Configuration Management Databases, external Security Dashboards and ITSM platforms.
The following types of information are sent:
 
 - Ohai data gathered from each node being managed, this includes HW, OS and installed program information, there is some variation depending on the OS being managed
 - Configuration information about each node being managed, this includes Chef Client Run status, Runlists, Cookbooks, Recipes being run against each node
 - Compliance information about each node that shows the compliance state, this includes passed and failed controls for each profile executed against that node

A Data Feed is not a Data Tap, it operates by doing the following:

- Every 4 hours (configurable) the data-feed-service will aggregate the client runs and compliance reports from the previous 4 hours and send this to the registered destinations.
- If there are no destinations, aggregation will not occur.
- The data is aggregated and sent in batches of 50 nodes at a time (also configurable)


By default only Admins of Chef Automate may create and manage Data Feeds.

### Adding a Data Feed Endpoint

To add an endpoint for a Data Feed in Chef Automate:

![Setup Data Feed Page](/images/docs/filled_form_create_data_feed.png)

1. In the **Settings** tab, navigate to the _Data Feeds_ page in the sidebar.
1. Select **Create Data Feed**.
1. Enter a unique Data Feed name.
1. Enter the URL for your Data Feed End Point, including any specific port details
1. Enter the Username and Password that your 3rd party endpoint requires for Authentication
1. Use the **Test Data Feed** button to validate the connection details
1. Once the Test is successful, use the **Create Data Feed** button to save your Data Feed Configuration

### Edit a Data Feed Endpoint

To edit an Endpoint for a Data Feed in Chef Automate:

1. From the _Data Feeds_ page, select the Data Feed name to open its detail page.
1. Edit the Data Feed name or URL.
1. Use the **Save** button to save the Data Feed.

### Delete a Data Feed Endpoint

To delete an Endpoint for a Data Feed in Chef Automate:

1. From the _Data Feeds_ page, select **Delete Data Feed** from the menu at the end of the table row.
1. Confirm that you wish to permanently delete this Data Feed by using the **Delete Data Feed** button.

## Configuring Global Data Feed Behaviour

On the Chef Automate CLI

1. Navigate to /hab/svc/data-feed-service/config/config.toml
1. To change the interval for the Data Feed collection update the config parameter for *feed_interval* which defaults to 4 hours
1. To change the number of sets of node data sent in each individual batch to your end point update the config parameter for *node_batch_size* which defaults to 50 nodes
1. To determine what data to include in each export use the update_nodes_only setting. By default, only data that has recently changed will be aggregated. With update_nodes_only set to false additional data is aggregated with each node that has recently changed, all the latest data is sent.
1. To reduce the IP address range for the node data being collected and processed, if for example you only wish to send production or test node traffic, update the config parameter for *disable_cidr_filter* to false **and** update the setting for *cidr_filter* to cover the IP address range required

/hab/svc/data-feed-service/config/config.toml

    [service]

    host = "localhost"\
    port = 14001\
    feed_interval = "4h"\
    asset_page_size = 100\
    reports_page_size = 1000\
    node_batch_size = 50\
    updated_nodes_only = true\
    disable_cidr_filter = true\
    cidr_filter = "0.0.0.0/0"

To debug any issues with the Data Feed Service in Automate, update the following section by changing the *log_level* to debug:

    [log]
    log_format = "text"
    log_level = "debug"

## Data Feed Output Syntax and Details

The import consists of line separated JSON strings. Each line represents the data for one node. Each line is of the format:

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
    "report": {...},
    "client_run": {...},
    "node: {
     "ip_address" : "",
     "mac_address": "",
     "description":"",
     "serial_number":"",
     "os_service_pack":""
     }
    }


