+++
title = "Client Runs"
description = "Chef Client Run Status"
date = 2018-03-26T16:01:58-07:00
draft = false
bref = ""
toc = true
+++

## Overview

The client runs page shows all of the nodes connected to Chef Automate, either directly or via a Chef Server proxy. Nodes appear in this view after chef-client run has been executed.

## Chef Client Run Status Overview

The Chef Client Run Status chart displays a summary of node statuses: failed, successful, or missing, as well as the total node count. The chart changes as you select filters.

![Client Runs Overview](/images/docs/client-runs.png)

## Node List Table

The node list table shows all of the nodes connected to Automate. Filter the node list table by selecting any of the status tabs below the **Chef Client Run Status** box.
Sort the nodes listed on the table by selecting the arrows to the right of the column headers: _Node Name_, _Check-In_, _Uptime_, _Platform_, _Environment_ or _Policy Group_.
Selecting an entry in this table will take you to a Node details page with more information about the Chef client runs associated with this node.

It is possible for a node to be present in this table without any associated run history.
This happens when data retention settings erase the most recent run history for that node.
In this case, a **no data** icon appears and you will be unable to view any node details.
The node remains listed as a missing node until it is deleted from Automate.
Automate automatically removes any modes deleted from the Chef Server.

## Node Details

The node details table displays the most recent converge results.
You'll find more information about _Resources_, _Run List_, and _Attributes_ in the tabs below the node detail chart.
Select the tabs to switch between these three views.

_Resources_ displays the status of the most recent resources are failed, successful, unchanged and unprocessed.
Selecting the tabs with these names will filter the list to show only those resources.
_Run List_ shows cookbooks, roles and recipes. For a node using policyfiles, you will be able to see the policy ID's for each cookbook listed.
_Attributes_ shows an expandable list of node properties. Use the search bar to discover the node attributes by attribute name, key name, or value. The search results show by highlighting matching attributes. Use the _default_, _normal_, _override_, and _automatic_ buttons beneath the search bar to filter attributes by to these categories.
Learn more about [attributes](https://docs.chef.io/attributes.html)

When looking at a failed Chef client run, selecting "view error log" button at the top of the page opens a window showing the error message and backtrace. Use the downloaded button to save the error message.

Selecting a node from the node list table opens the node details page with the most recent information about that node.
To look at past run data, select the **Run History** button on the upper right, which opens a side panel containing historical run data. You can filter this data using the run status icons and by date range.
Node history data supports up to three months of client-run information. Scroll through the node history using the pagination buttons at the bottom of the side panel. Use the "X" button at the top of the panel to close the side panel.

## Filtering

### Search Bar

Filter nodes from the search bar based on existing node information. You can apply multiple filters to a search.
The node list table changes to display only nodes that satisfy all of the applied filters.
To apply a filter, first select the filter from the dropdown list and begin typing to display autocomplete options.
To save a search, select the share button to the right of the search bar, and copy the private URL.

#### Node Filters

[Attribute](https://docs.chef.io/attributes.html)
: Search for an attribute key, this will not search across attribute values.

[Cookbook](https://docs.chef.io/cookbooks.html)
: A cookbook name.

[Environment](https://docs.chef.io/environments.html)
: Nodes can have one environment.

[Node Name](https://docs.chef.io/nodes.html#about-node-names)
: Name of the node.

[Platform](https://docs.chef.io/platforms.html#chef-automate-server)
: OS Platform of a node.

[Policy Group](https://docs.chef.io/policyfile.html#settings)
: Policy group name, only nodes using policyfiles will be shown.

[Policy Name](https://docs.chef.io/policyfile.html#settings)
: Name of the policy as set in policyfile.rb, only nodes using policyfiles will be shown.

[Policy Revision](https://docs.chef.io/release_notes_server.html#policies-name-revisions)
: The policy revision ID, only nodes using policyfiles will be shown.

[Recipe](https://docs.chef.io/recipes.html)
: A recipe within a cookbook.

[Resource Name](https://docs.chef.io/resource_reference.html)
: A resource within a cookbook.

[Role](https://docs.chef.io/roles.html)
: Search by nodes assigned to a role. Nodes can have zero or many roles.

See more about [policyfiles](https://docs.chef.io/policyfile.html).

### Sidebar Filter

Find nodes on the basis of their related Chef Server or Chef Server organization from the sidebar on the left side of the screen.
The sidebar shows all Chef Servers and all available Chef Server organizations.

## Managing Node Data

### Managing Missing Nodes

Configure the timing for marking nodes as missing and then deleting them from [Node Lifecycle]({{< relref "node-lifecycle.md" >}}) on the Settings tab.

### Deleting Missing Nodes

Admins and users with the relevant permissions defined in access policies, can delete missing nodes from the Chef Client Runs page. Only missing nodes can be deleted; active nodes cannot be deleted.

To delete one or more missing nodes, tick the checkbox to the left of the node name and then select the red delete button above the table header. Confirm the delete action in the pop-up window.

To delete all missing nodes, tick the checkbox at the top of the client runs table, which selects all of the All missing nodes on the current page will then be selected. The user can choose to deselect individual nodes by unchecking the checkboxes next to nodes. Select the delete button and confirm the delete action in the pop-up window.

### Deleting Missing Nodes from the Command Line

Nodes can also be deleted with the Chef Automate CLI or through the Chef Automate REST API.

To delete a node from the _Client Runs_ page using the Chef Automate CLI, first locate the `node ID` on the _Node Details_ page and use this ID with the `node-delete` command:

```bash
chef-automate infrastructure node-delete 3f2a2830-0ef3-474a-a835-3a7dd25361fe
```

To delete nodes using the REST API, use the `"https://automate-url/api/v0/ingest/events/chef/nodedelete"` endpoint to delete a single node, or the `"https://automate-url/api/v0/ingest/events/chef/node-multiple-deletes"` endpoint to delete multiple nodes.
Identify your node or nodes, with either the _node\_id_ --which is the UUID of the node as it appears in Automate--or the combination of _node name_, _organization name_, and _service hostname_.
The _service hostname_ is the `fqdn` of your Chef Server or, in the case of chef-solo nodes, `localhost`.

#### Request for deleting a node using the _node\_id_

```bash
curl -sSX POST "https://automate-url/api/v0/ingest/events/chef/nodedelete" -d
'{
  "node_id": "3f2a2830-0ef3-474a-a835-3a7dd25361fe"
}'
-H "X-Data-Collector-Token: $TOKEN"
```

#### Request for deleting multiple nodes using the _node\_id_

```bash
curl -sSX POST "https://automate-url/api/v0/ingest/events/chef/node-multiple-deletes" -d
'{
  "node_ids": ["3f2a2830-0ef3-474a-a835-3a7dd25361fe", "9c139ad0-89a5-44bc-942c-d7f248b155ba"]
}'
-H "X-Data-Collector-Token: $TOKEN"
```

#### Request for deleting a node using the _node name_, _organization name_, and _service hostname_

```bash
curl -sSX POST "https://automate-url/api/v0/ingest/events/chef/nodedelete" -d
'{
  "node_name": "somenode",
  "organization_name": "yourorg",
  "service_hostname": "chef-server-fqdn"
}'
-H "X-Data-Collector-Token: $TOKEN"
```

### Managing Ephemeral Nodes

Automate considers the instances of ephemeral nodes, which are nodes that are frequently created and destroyed, as new nodes by default, even if the node repeatedly uses the same name.
Set Automate to consider ephemeral nodes as manifestations of the same node by configuring the UUID on the client side.
Configuring the UUID on the client side keeps the node associated with the same id, which makes Automate consider it as the same node every time it is recreated.
In the node's `client.rb`, set `chef_guid` to the _desired UUID_.
If the node already exists, check that it uses the correct UUID, otherwise it will appear as a new node the next time it is recreated.

See the `client.rb` documentation for more information about [configuring your client nodes](https://docs.chef.io/config_rb_client.html).

The following are the configuration parameters available:

| Parameter | Type | Explanation | Format | Default |
| --------- | ---- | ----------- | ------- | ------ |
|`threshold`|string|The duration after which unreported nodes are marked as missing.|`1h30m`, `1m`, `2h30m`, `1d`, etc.|`1d`|
|`every`|string|How often to scan the nodes to check if they are missing.|`1h30m`, `1m`, `2h30m`, `1d`, etc.|`15m`|
|`running`|boolean|Is the job running? Set to false to turn off missing node functionality.|n/a|`true`|

Below is an example curl command:

```bash
curl -sSX POST "https://automate-url/api/v0/retention/nodes/missing-nodes/config" -d
'{
  "threshold": "1d",
  "every": "15m",
  "running": true
}'
-H "api-token: $TOKEN"
```

You will need an API token to send API requests. You can either [create a new token]({{< relref "api-tokens.md#creating-a-standard-api-token" >}}) or [use an Automate 1 data collector token]({{< relref "data-collection.md#existing-data-collector-token-setup" >}}) if you are migrating from Chef Automate 1.

### Configuring Data Cleanup

By default, Automate prevents irreversible destructive operations by keeping deleted node history in Elasticsearch, unless users configure this functionality.
Chef recommends setting the `threshold` for destroying deleted node history to 1 day and running data cleanup every 15 minutes.

Available data cleanup configuration parameters:

| Parameter | Type | Explanation | Format | Default |
| --------- | ---- | ----------- | ------- | ------ |
|`threshold`|string|The duration after which nodes marked for deletion are removed.|`1h30m`, `1m`, `2h30m`, `1d`, etc.|`1d`|
|`every`|string|How often to scan for marked nodes for deletion to be removed.|`1h30m`, `1m`, `2h30m`, `1d`, etc.|`15m`|
|`running`|boolean|Is the job running, set to true to turn on data cleanup functionality.|n/a|`false`|

Below is an example curl command with the recommended data cleanup settings:

```bash
curl -sSX POST "https://automate-url/api/v0/retention/nodes/delete-nodes/config" -d
'{
  "threshold": "1d",
  "every": "15m",
  "running": true
}'
-H "api-token: $TOKEN"
```

You will need an API token to send API requests. You can either [create a new token]({{< relref "api-tokens.md#creating-a-standard-api-token" >}}) or use a [Chef Automate 1 data collector token]({{< relref "data-collection.md#existing-data-collector-token-setup" >}}) if you are migrating from Chef Automate 1.
